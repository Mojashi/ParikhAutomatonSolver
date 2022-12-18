package xyz.mojashi
package automaton

import formula.{Predicate}

case class ParikhAutomaton [In, State, Label, Value]
(
  voa: VectorOutputTransducer[In, State, Label, Value],
  constraint: Predicate[Label, Value],
)
