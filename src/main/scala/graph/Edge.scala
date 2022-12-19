package com.github.Mojashi
package graph

trait Edge[State] {
  val from: State
  val to: State
  val id: EdgeID
}

case class EdgeImpl[State](
  from: State,
  to: State,
  id: EdgeID
) extends Edge[State]()

object UniqueEdgeId {
  var id = 0
  def get() = {
    id += 1
    id
  }
}