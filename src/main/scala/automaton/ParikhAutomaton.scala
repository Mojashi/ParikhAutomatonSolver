package com.github.Mojashi
package automaton

import formula.{Predicate}

case class ParikhAutomaton [In, State, Label, Value]
(
  voa: VectorOutputTransducer[In, State, Label, Value],
  constraint: Predicate[Label, Value],
)
