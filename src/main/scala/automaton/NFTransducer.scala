package xyz.mojashi
package automaton

trait NFTransducer[In, Out, State, T <: TransducerTransition[Option[In], Out, State]] extends NFA[In, State, T] {
  def run(in: Seq[In]): Set[Seq[Out]]
}
