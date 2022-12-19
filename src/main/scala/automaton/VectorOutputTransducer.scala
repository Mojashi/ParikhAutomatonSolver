package com.github.Mojashi
package automaton

trait VectorOutputTransducer[In, Label, Value] extends NFTransducer[In, Map[Label, Value], TransducerTransition[In, Map[Label, Value]]] {
  def runWithAddition(in: Seq[In]): Set[Map[Label, Value]]
}
