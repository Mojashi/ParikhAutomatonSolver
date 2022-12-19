package com.github.Mojashi
package automaton

import graph.{EdgeID, StateID}

trait TransducerTransition[In, Out] extends Transition[In] {
  def out: Option[Out]
}

case class TransducerTransitionImpl[In, Out]
(
  out: Option[Out],
  in: Option[In],
  from: StateID,
  to: StateID,
  id: EdgeID
) extends TransducerTransition[In, Out]
