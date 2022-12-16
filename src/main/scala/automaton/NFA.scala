package xyz.mojashi
package automaton

import graph.Graph

trait NFA[In, State, T <: Transition[Option[In], State]] extends Graph [State, T] {
  val start: State
  val fin: State

  def accept(in: Seq[In]): Boolean
}
