package xyz.mojashi
package automaton

case class ParikhAutomaton [In, State, Label, Value]
(
  voa: VectorOutputTransducer[In, State, Label, Value],
  constraint: Seq[AtomPredicate[Label, Value]],
)
