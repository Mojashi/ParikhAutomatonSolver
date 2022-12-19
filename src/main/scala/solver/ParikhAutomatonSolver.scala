package com.github.Mojashi
package solver

import automaton.ParikhAutomaton

import com.github.Mojashi.formula.{AtomPredicate, Expression, Predicate}
import com.github.Mojashi.graph.{Edge, EdgeID}


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
