package com.github.Mojashi
package graph

case class GraphImpl[E <: Edge]
(
  override val transitions: Seq[E]
) extends Graph[E] {

  override val states: Seq[StateID] = transitions.flatMap(t => Set(t.from, t.to)).toSet.toSeq

  private val transitionMap: Map[EdgeID, E] = transitions.map(t=>(t.id, t)).toMap
  private val sourceFromMap: Map[StateID, Seq[E]] = transitions.groupBy(t=>t.from)
  private val targetToMap: Map[StateID, Seq[E]] = transitions.groupBy(t=>t.to)
  override def findTransitionByID(id: EdgeID): Option[E] = transitionMap.get(id)

  override def sourceFrom(s: StateID): Seq[E] = sourceFromMap.getOrElse(s, Seq())

  override def targetTo(s: StateID): Seq[E] = targetToMap.getOrElse(s, Seq())
}
