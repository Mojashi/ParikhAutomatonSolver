package com.github.Mojashi
package automaton.impl

import automaton.{TransducerTransition, VectorOutputTransducer}

import com.github.Mojashi.graph.StateID
import com.github.Mojashi.utils.addVector

class VectorOutputTransducerImpl[In, Label, Value: Numeric]
(
  start: StateID,
  fin: StateID,
  transitions: Seq[TransducerTransition[In, Map[Label, Value]]],
) extends NFTransducerImpl[In, Map[Label, Value], TransducerTransition[In, Map[Label, Value]]] (
  start, fin, transitions
)
with VectorOutputTransducer[In, Label, Value]
{
  override def runWithAddition(in: Seq[In]): Set[Map[Label, Value]] = {
    run(in).map(s => s.fold(Map())( (sum, cur) =>
      addVector(sum, cur)
    ))
  }
}
