package xyz.mojashi
package solver.mp

import solver.ParikhAutomatonSolver

import com.google.ortools.linearsolver.MPSolver

class MIPBasedSolver[In, State, Label, Value]
(
  val pa: ParikhAutomaton[In, State, Label, Value]
) extends ParikhAutomatonSolver[In, State, Label, Value] {
  val mpSolver = MPSolver.createSolver("SCIP")

  override def solve(): Option[Map[Int, Value]] = ???

  val objective = Constant(0)

  override def setObjective(minimize: Expression[Label, Value]): Unit = {
    val (coeffs, constant) = getCoefficients(minimize)
    val obj = mpSolver.objective()
    obj.clear()

    for ((v, c) <- coeffs) {
      obj.setCoefficient(obj.getCoefficient(mpSolver.lookupVariableOrNull()) v, c)
    }

    .setMinimization()
    mpSolver.objective().setCoefficient()
  }

  def getVariable(v:)

  override def addConstraint(constraint: AtomPredicate[Label, Value], constraintID: ConstraintID): ConstraintID = {
//    mpSolver
  }

  override def removeConstraint(constraintID: ConstraintID): Unit = ???

}
