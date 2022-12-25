package com.github.Mojashi
package automaton.impl

import automaton.{NFTransducer, TransducerTransition, VectorOutputTransducer}

object ToVectorOutputTransducerConv {
  implicit def ToVectorOutputTransducerConv[In, Label, Value: Numeric]
  (
    t: NFTransducer[In, Map[Label, Value], TransducerTransition[In, Map[Label, Value]]]
  ): VectorOutputTransducer[In, Label, Value] = {
    new VectorOutputTransducerImpl(
      start = t.start,
      fin = t.fin,
      transitions = t.transitions
    )
  }
}
