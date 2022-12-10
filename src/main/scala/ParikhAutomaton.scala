package xyz.mojashi

case class ParikhAutomaton [In, State, Label, Value]
(
  voa: VectorOutputAutomaton[In, State, Label, Value],
  constraint: Seq[AtomPredicate[Label, Value]],
)
