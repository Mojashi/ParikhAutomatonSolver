package com.github.Mojashi
package graph

trait Graph[+E <: Edge] {
  def transitions: Seq[E]
  val states: Seq[StateID]

  def findTransitionByID(id: EdgeID): Option[E]
  def sourceFrom(s: StateID): Seq[E]
  def targetTo(s: StateID): Seq[E]
}
