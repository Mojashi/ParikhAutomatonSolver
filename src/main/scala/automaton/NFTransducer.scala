package com.github.Mojashi
package automaton

trait NFTransducer[In, Out, +T <: TransducerTransition[In, Out]] extends NFA[In, T] {
  def run(in: Seq[In]): Set[Seq[Out]]
}
