package com.github.Mojashi
package solver

import automaton.ParikhAutomaton

import com.github.Mojashi.formula.{AtomPredicate, Expression, Predicate}
import com.github.Mojashi.graph.{Edge, EdgeID}


trait ParikhAutomatonSolver[In, Label, Value] {
  def solve(): Option[Map[EdgeID, Double]]

  val pa: ParikhAutomaton[In, Label, Value]
  def setObjective(minimize: Expression[Either[Label, EdgeID], Value])

  type ConstraintID = String
  def addConstraint(constraint: Predicate[Label, Value], constraintID: ConstraintID = ""): Seq[ConstraintID]
  def addAtomPredicateConstraint(constraint: AtomPredicate[Label, Value], constraintID: ConstraintID = ""): ConstraintID = {
    val r = addConstraint(constraint, constraintID)
    r.head
  }

  def removeConstraint(constraintID: ConstraintID)
}
