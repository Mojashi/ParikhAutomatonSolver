package com.github.Mojashi
package automaton

import graph.{Edge, EdgeID}

trait Transition[In, State] extends Edge[State] {
  def in: In
}

case class TransitionImpl[In, State]
(
  in: In,
  from: State,
  to: State,
  id: EdgeID
) extends Transition[In, State]
