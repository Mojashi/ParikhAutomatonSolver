package com.github.Mojashi
package automaton.impl

import automaton.{TransducerTransition, VectorOutputTransducer}

import com.github.Mojashi.utils.addVector

class VectorOutputTransducerImpl[In, State, Label, Value: Numeric]
(
  start: State,
  fin: State,
  transitions: Seq[TransducerTransition[In, Map[Label, Value], State]],
) extends NFTransducerImpl[In, Map[Label, Value], State, TransducerTransition[In, Map[Label, Value], State]] (
  start, fin, transitions
)
with VectorOutputTransducer[In, State, Label, Value]
{
  override def runWithAddition(in: Seq[In]): Set[Map[Label, Value]] = {
    run(in).map(s => s.fold(Map())( (sum, cur) =>
      addVector(sum, cur)
    ))
  }
}
