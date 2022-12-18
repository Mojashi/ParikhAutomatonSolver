package xyz.mojashi
package solver

import automaton.ParikhAutomaton

import xyz.mojashi.formula.{AtomPredicate, Expression, Predicate}
import xyz.mojashi.graph.{Edge, EdgeID}


trait ParikhAutomatonSolver[In, State, Label, Value] {
  def solve(): Option[Map[EdgeID, Double]]

  val pa: ParikhAutomaton[In, State, Label, Value]
  def setObjective(minimize: Expression[Label, Value])

  type ConstraintID = String
  def addConstraint(constraint: Predicate[Label, Value], constraintID: ConstraintID = ""): ConstraintID
  def addAtomPredicateConstraint(constraint: AtomPredicate[Label, Value], constraintID: ConstraintID = ""): ConstraintID =
    addConstraint(constraint, constraintID)
  def removeConstraint(constraintID: ConstraintID)
}
