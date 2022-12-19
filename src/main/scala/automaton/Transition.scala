package com.github.Mojashi
package automaton

import graph.{Edge, EdgeID, StateID}

trait Transition[In] extends Edge {
  def in: Option[In]
}

case class TransitionImpl[In]
(
  in: Option[In],
  from: StateID,
  to: StateID,
  id: EdgeID
) extends Transition[In]
