package com.github.Mojashi
package automaton

trait NFTransducer[In, Out, State, +T <: TransducerTransition[In, Out, State]] extends NFA[In, State, T] {
  def run(in: Seq[In]): Set[Seq[Out]]
}
