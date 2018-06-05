package oxid

object encoder {

  import inox.{trees => i}
  import inox.InoxProgram

  def apply(program: Program): InoxProgram = {
    InoxProgram(program.defs.map(apply), Seq.empty)
  }

  def apply(fd: FunDef): i.FunDef = {
    new i.FunDef(
      apply(fd.id),
      Seq.empty,
      fd.params.map(apply),
      apply(fd.returnType),
      apply(fd.body),
      Seq.empty
    )
  }

  def apply(id: Identifier): inox.Identifier = {
    new inox.Identifier(id.name, id.name.hashCode, 0)
  }

  def apply(vd: ValDef): i.ValDef = {
    i.ValDef(apply(vd.id), apply(vd.tpe))
  }

  def apply(tpe: Type): i.Type = tpe.baseType match {
    case FunctionType(from, to) => i.FunctionType(from.map(apply), apply(to))
    case IntType => i.IntegerType()
    case BooleanType => i.BooleanType()
    case _ => sys.error("unreachable")
  }

  def apply(expr: Expr): i.Expr = expr match {
    case BooleanLiteral(value) =>
      i.BooleanLiteral(value)

    case IntLiteral(value) =>
      i.IntegerLiteral(value)

    case Variable(id, tpe) =>
      i.Variable(apply(id), apply(tpe), Seq.empty)

    case Let(vd, value, body) =>
      i.Let(apply(vd), apply(value), apply(body))

    case Plus(left, right) =>
      i.Plus(apply(left), apply(right))

    case Equals(left, right) =>
      i.Equals(apply(left), apply(right))

    case LessThan(left, right) =>
      i.LessThan(apply(left), apply(right))

    case And(left, right) =>
      i.And(apply(left), apply(right))

    case Or(left, right) =>
      i.Or(apply(left), apply(right))

    case Implies(left, right) =>
      i.Implies(apply(left), apply(right))

    case IfThenElse(cnd, thn, els) =>
      i.IfExpr(apply(cnd), apply(thn), apply(els))

    case Lambda(vd, body) =>
      i.Lambda(Seq(apply(vd)), apply(body))

    case Application(lhs, rhs) =>
      i.Application(apply(lhs), Seq(apply(rhs)))

    case FunctionInvocation(id, args) =>
      i.FunctionInvocation(apply(id), Seq.empty, args.map(apply))
  }


}
