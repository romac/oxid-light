
package oxid

object test {

  val f = FreshIdentifier("f")
  val g = FreshIdentifier("g")
  val h = FreshIdentifier("h")

  val x = ValDef(FreshIdentifier("x"), IntType)
  val y = ValDef(FreshIdentifier("x"), IntType)
  val z = ValDef(FreshIdentifier("z"), IntType)
  val r = ValDef(FreshIdentifier("r"), IntType)

  val fDef = FunDef(
    f,
    Seq(
      ValDef(
        x.id,
        RefinedType(
          x,
          Equals(Plus(x.toVariable, IntLiteral(1)), IntLiteral(10))
        )
      )
    ),
    Plus(x.toVariable, IntLiteral(1)),
    RefinedType(r, Equals(r.toVariable, IntLiteral(10)))
  )

  val gDef = FunDef(
    g,
    Seq(
      ValDef(
        y.id,
        RefinedType(
          y,
          LessThan(y.toVariable, IntLiteral(10))
        )
      )
    ),
    Plus(y.toVariable, IntLiteral(2)),
    RefinedType(r, LessThan(r.toVariable, IntLiteral(11)))
  )

  val hDef = FunDef(
    h,
    Seq(
      ValDef(
        z.id,
        RefinedType(
          z,
          Equals(Plus(z.toVariable, IntLiteral(1)), IntLiteral(10))
        )
      )
    ),
    Plus(FunctionInvocation(f, Seq(Plus(z.toVariable, IntLiteral(1)))), IntLiteral(1)),
    RefinedType(r, Equals(r.toVariable, IntLiteral(10)))
  )

  val program = Program(Seq(fDef, gDef, hDef))

  def main(args: Array[String]): Unit = {
    println()
    println(program)
    println()

    val vcs = TypeCheck(program).check()
    val solver = Solver(program)
    vcs foreach { case VC(id, cond) =>
      val expr = encoder(cond)
      println(s"Solving VC for '$id': $expr")
      val res = solver.solve(expr)
      println(s"=> $res\n")
    }
  }

}

