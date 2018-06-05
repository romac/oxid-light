
package oxid

case class TypeError(msg: String) extends Exception(s"Type error: $msg")

case class VC(name: String, condition: Expr)

case class TypeCheck(program: Program) {

  var vcs: Set[VC] = Set.empty

  def getFunction(id: Identifier): FunDef = program.defs.collectFirst {
    case fd: FunDef if fd.id == id => fd
  }.get

  def check(): Set[VC] = {
    vcs = Set.empty
    program.defs.map(check)
    vcs
  }

  def addVC(vc: VC): Unit = {
    println(vc.condition)
    vcs = vcs + vc
  }

  def check(defn: Definition): Unit = defn match {
    case ValDef(vd, tpe) => ()
    case FunDef(id, params, body, returnType) =>
      val bindings = params.map(vd => vd.id -> vd.toVariable).toMap
      val bodyType = infer(body)(bindings)
      val pre = params.filter(_.tpe.isRefined).foldLeft(BooleanLiteral(true): Expr) {
        case (acc, vd) => And(acc, vd.tpe.refined.pred)
      }
      val post = returnType.refined.toLambda
      val res = ValDef(FreshIdentifier("res"), bodyType)
      val condition = Implies(pre, Let(res, body, Application(post, res.toVariable)))

      addVC(VC(id.toString, condition))
  }

  def addSubtypingCheck(expr: Expr, exprType: Type, tpe: Type)(implicit bindings: Map[Identifier, Expr]): Unit = {
    def isTrivial(vd: ValDef, expr: Expr): Boolean = (vd, expr) match {
      case (ValDef(vdId, _), Variable(id, _)) => vdId == id
      case _ => false
    }

    val pre = bindings.foldLeft(BooleanLiteral(true): Expr) {
      case (acc, (id, value)) =>
        val vd = ValDef(id, value.getType)
        val clause =
          if (isTrivial(vd, expr))
            value.getType.refined.pred
          else
            And(Equals(vd.toVariable, value), value.getType.refined.pred)

        And(acc, clause)
    }

    val post = tpe.refined.toLambda
    val res = ValDef(FreshIdentifier("res"), exprType)
    val condition = Implies(pre, Let(res, expr, Application(post, res.toVariable)))

    addVC(VC("subtype", condition))
  }

  def isInt(tpe: Type): Boolean     = tpe.baseType == IntType
  def isBoolean(tpe: Type): Boolean = tpe.baseType == BooleanType

  def isCompatible(t1: Type, t2: Type): Boolean = {
    t1.baseType == t2.baseType
  }

  def infer(expr: Expr)(implicit bindings: Map[Identifier, Expr]): Type = expr match {
    case b: BooleanLiteral =>
      expr.setType(BooleanType.refine(b))

    case i: IntLiteral =>
      expr.setType(IntType.refine(i))

    case Variable(id, tpe) if (!bindings.contains(id)) =>
      throw TypeError(s"Unbound variable '$id'")

    case Variable(id, tpe) => tpe

    case Let(vd, value, body) =>
      val valueType = infer(value)

      if (!isCompatible(vd.tpe, valueType))
        expr.setType(Untyped)
      else
        expr.setType(infer(body)(bindings + (vd.id -> value)))

    case Plus(left, right) => expr.setType {
      if (isInt(infer(left)) && isInt(infer(right)))
        IntType.refine(Plus(left, right))
      else
        Untyped
    }

    case Equals(left, right) => expr.setType {
      val l = infer(left)
      val r = infer(left)

      if (l.baseType == r.baseType)
        BooleanType.refine(Equals(left, right))
      else
        Untyped
    }

    case LessThan(left, right) => expr.setType {
      val l = infer(left)
      val r = infer(left)

      if (l.baseType == r.baseType)
        BooleanType.refine(Equals(left, right))
      else
        Untyped
    }

    case And(left, right) => expr.setType {
      if (isBoolean(infer(left)) && isBoolean(infer(right)))
        BooleanType.refine(And(left, right))
      else
        Untyped
    }

    case Or(left, right) => expr.setType {
      if (isBoolean(infer(left)) && isBoolean(infer(right)))
        BooleanType.refine(Or(left, right))
      else
        Untyped
    }

    case Implies(left, right) => expr.setType {
      if (isBoolean(infer(left)) && isBoolean(infer(right)))
        BooleanType.refine(Implies(left, right))
      else
        Untyped
    }

    case IfThenElse(cnd, thn, els) => expr.setType {
      val t = infer(thn)
      val e = infer(els)

      if (isBoolean(infer(cnd)) && isCompatible(t, e))
        t.baseType.refine(v => Or(Equals(v, thn), Equals(v, els)))
      else
        Untyped
    }

    case Lambda(vd, body) => expr.setType {
      FunctionType(Seq(vd.tpe), infer(body))
    }

    case Application(lhs, rhs) => expr.setType {
      val rhsType = infer(rhs)

      infer(lhs) match {
        case FunctionType(Seq(fromType), toType) if isCompatible(fromType, rhsType) =>
          addSubtypingCheck(rhs, rhsType, fromType)
          toType

        case _ => Untyped
      }
    }

    case FunctionInvocation(id, args) => expr.setType {
      val fn = getFunction(id)

      val ok = fn.params.zip(args) forall { case (vd, arg) =>
        val argType = infer(arg)
        val compatible = isCompatible(argType, vd.tpe)

        if (compatible) addSubtypingCheck(arg, argType, vd.tpe)

        compatible
      }

      if (ok)
        fn.returnType
      else
        Untyped
    }
  }

}

