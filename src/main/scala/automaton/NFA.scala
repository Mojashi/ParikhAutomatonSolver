package com.github.Mojashi
package automaton

import graph.{Graph, StateID}

trait NFA[In, +T <: Transition[In]] extends Graph [T] {
  val start: StateID
  val fin: StateID

  def accept(in: Seq[In]): Boolean
}
