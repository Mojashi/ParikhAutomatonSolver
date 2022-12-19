package com.github.Mojashi
package graph

trait Edge {
  val from: StateID
  val to: StateID
  val id: EdgeID
}

case class EdgeImpl(
  from: StateID,
  to: StateID,
  id: EdgeID
) extends Edge

object UniqueEdgeId {
  var id = 0
  def get(): EdgeID = {
    id += 1
    id.toString
  }
}