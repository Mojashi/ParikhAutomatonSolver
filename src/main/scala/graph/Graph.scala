package xyz.mojashi
package graph

trait Graph[State, +E <: Edge[State]] {
  def transitions: Seq[E]
  val states: Seq[State]

  def findTransitionByID(id: EdgeID): Option[E]
  def sourceFrom(s: State): Seq[E]
  def targetTo(s: State): Seq[E]
}
