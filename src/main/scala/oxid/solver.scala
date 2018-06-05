
package oxid

import inox.solvers.{SimpleSolverAPI, SolverResponses, SolverFactory}

sealed abstract class Result
object Result {
  final case object Valid extends Result
  final case object Unknown extends Result
  final case class Invalid(model: inox.Model) extends Result
}

case class Solver(program: Program) {
  import SolverResponses._

  val inoxProgram = encoder(program)
  import inoxProgram.trees._

  val ctx = inox.Main.setup(Array.empty)
  val solver = SimpleSolverAPI(inoxProgram.getSolver(ctx).withTimeout(5000))

  def solve(expr: Expr): Result = {
    solver.solveSAT(Not(expr)) match {
      case SatWithModel(model) => Result.Invalid(model)
      case Unsat               => Result.Valid
      case Unknown             => Result.Unknown
    }

  }

}
