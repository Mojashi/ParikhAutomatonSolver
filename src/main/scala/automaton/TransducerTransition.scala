package xyz.mojashi
package automaton

import graph.EdgeID

trait TransducerTransition[In, Out, State] extends Transition[In, State] {
  def out: Out
}

case class TransducerTransitionImpl[In, Out, State]
(
  out: Out,
  in: In,
  from: State,
  to: State,
  id: EdgeID
) extends TransducerTransition[In, Out, State]
