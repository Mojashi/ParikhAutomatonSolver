package xyz.mojashi
package solver


trait ParikhAutomatonSolver[In, State, Label, Value] {
  def solve(): Option[Map[Edge[State]#EdgeID, Double]]

  val pa: ParikhAutomaton[In, State, Label, Value]
  def setObjective(minimize: Expression[Label, Value])

  type ConstraintID = String
  def addConstraint(constraint: AtomPredicate[Label, Value], constraintID: ConstraintID = ""): ConstraintID
  def removeConstraint(constraintID: ConstraintID)
}