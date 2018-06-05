
package oxid

final case class Program(defs: Seq[FunDef]) {
  override def toString = defs.mkString("\n\n")
}

final case class Identifier(name: String, globalId: Int) {
  override def toString = name
  def uniqueName = s"$name$$$globalId"

  def freshen: Identifier = FreshIdentifier(name)
}

object FreshIdentifier {
  private var nextGlobalId: Int = 1

  def apply(name: String): Identifier = {
    val ident = Identifier(name, nextGlobalId)
    nextGlobalId += 1
    ident
  }
}

sealed abstract class Type {
  def baseType: Type
  def isRefined: Boolean
  def refined: RefinedType = RefinedType(ValDef.fresh("_", this), BooleanLiteral(true))
  def toString: String

  def refine(q: Variable => Expr): Type = this match {
    case Untyped =>
      Untyped

    case RefinedType(vd, p) =>
      RefinedType(vd, And(p, q(vd.toVariable)))

    case other =>
      val vd = ValDef.fresh("x", this)
      RefinedType(vd, q(vd.toVariable))
  }

  def refine(value: Expr): Type = refine(v => Equals(v, value))
}

final case object Untyped extends Type {
  val baseType = this
  override val isRefined = false
  override def refined = sys.error("unreachable")
  override def toString = "<untyped>"
}

final case class FunctionType(from: Seq[Type], to: Type) extends Type {
  val baseType = this
  val isRefined = false
  override def toString = s"(${from.mkString(", ")}) => $to"
}

final case class RefinedType(vd: ValDef, pred: Expr) extends Type {
  override val refined = this
  val isRefined = true
  val baseType = vd.tpe
  def toLambda = Lambda(vd, pred)
  override def toString = s"{ $vd | $pred }"
}

final case object BooleanType extends Type {
  val baseType = this
  val isRefined = false
  override def toString = "Boolean"
}

final case object IntType extends Type {
  val baseType = this
  val isRefined = false
  override def toString = "Int"
}

sealed abstract class Expr {
  def toString: String

  private var _tpe: Type = Untyped

  def typed(tpe: Type): this.type = {
    setType(tpe)
    this
  }

  def setType(tpe: Type): Type = {
    _tpe = tpe
    tpe
  }

  def getType: Type = _tpe
}

final case class BooleanLiteral(value: Boolean) extends Expr {
  override def toString = s"$value"
}

final case class IntLiteral(value: Int) extends Expr {
  override def toString = s"$value"
}

final case class Variable(id: Identifier, tpe: Type) extends Expr {
  def toVal: ValDef = ValDef(id, tpe)
  override def getType: Type = tpe
  override def toString = s"$id"
}

final case class Let(vd: ValDef, value: Expr, body: Expr) extends Expr {
  override def toString = s"let $vd = $value in $body"
}

final case class Plus(left: Expr, right: Expr) extends Expr {
  override def toString = s"$left + $right"
}

final case class Equals(left: Expr, right: Expr) extends Expr {
  override def toString = s"$left == $right"
}

final case class LessThan(left: Expr, right: Expr) extends Expr {
  override def toString = s"$left < $right"
}

final case class And(left: Expr, right: Expr) extends Expr {
  override def toString = s"$left && $right"
}

final case class Or(left: Expr, right: Expr) extends Expr {
  override def toString = s"$left || $right"
}

final case class Implies(left: Expr, right: Expr) extends Expr {
  override def toString = s"$left ==> $right"
}

final case class IfThenElse(cnd: Expr, thn: Expr, els: Expr) extends Expr {
  override def toString = s"if ($cnd) $thn else $els"
}

final case class Lambda(vd: ValDef, body: Expr) extends Expr {
  override def toString = s"(($vd) => $body)"
}

final case class Application(lhs: Expr, rhs: Expr) extends Expr {
  override def toString = s"$lhs($rhs)"
}

final case class FunctionInvocation(id: Identifier, args: Seq[Expr]) extends Expr {
  override def toString = s"$id(${args.mkString(", ")})"
}

sealed abstract class Definition {
  val id: Identifier
}

final case class ValDef(id: Identifier, tpe: Type) extends Definition {
  def toVariable: Variable = Variable(id, tpe)
  def getType: Type = tpe

  override def toString =
    if (tpe.isRefined && tpe.refined.vd.id == id) {
      s"$id: ${tpe.baseType} { ${tpe.refined.pred} }"
    } else {
      s"$id: $tpe"
    }
}

object ValDef {
  def fresh(name: String, tpe: Type): ValDef = ValDef(FreshIdentifier(name), tpe)
}

final case class FunDef(
  id: Identifier,
  params: Seq[ValDef],
  body: Expr,
  returnType: Type,
) extends Definition {
  override def toString =
    s"""|fn $id(${params.mkString(", ")}): $returnType = {
        |  $body
        |}""".stripMargin
}

