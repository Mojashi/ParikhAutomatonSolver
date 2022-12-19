package com.github.Mojashi
package automaton

trait VectorOutputTransducer[In, State, Label, Value] extends NFTransducer[In, Map[Label, Value], State, TransducerTransition[Option[In], Map[Label, Value], State]] {
  def runWithAddition(in: Seq[In]): Set[Map[Label, Value]]
}
