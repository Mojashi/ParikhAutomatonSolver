package com.github.Mojashi
package automaton

import formula.{Predicate}

case class ParikhAutomaton [In, Label, Value]
(
  voa: VectorOutputTransducer[In, Label, Value],
  constraint: Predicate[Label, Value],
)
