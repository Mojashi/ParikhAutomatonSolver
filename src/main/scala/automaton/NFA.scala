package com.github.Mojashi
package automaton

import graph.Graph

trait NFA[In, State, +T <: Transition[In, State]] extends Graph [State, T] {
  val start: State
  val fin: State

  def accept(in: Seq[In]): Boolean
}
