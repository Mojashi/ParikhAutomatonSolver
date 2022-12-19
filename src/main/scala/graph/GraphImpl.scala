package com.github.Mojashi
package graph

class GraphImpl[State, E <: Edge[State]]
(
  override val transitions: Seq[E]
) extends Graph[State, E] {

  override val states: Seq[State] = transitions.flatMap(t => Set(t.from, t.to)).toSet.toSeq

  private val transitionMap: Map[EdgeID, E] = transitions.map(t=>(t.id, t)).toMap
  private val sourceFromMap: Map[State, Seq[E]] = transitions.groupBy(t=>t.from)
  private val targetToMap: Map[State, Seq[E]] = transitions.groupBy(t=>t.to)
  override def findTransitionByID(id: EdgeID): Option[E] = transitionMap.get(id)

  override def sourceFrom(s: State): Seq[E] = sourceFromMap.getOrElse(s, Seq())

  override def targetTo(s: State): Seq[E] = targetToMap.getOrElse(s, Seq())
}
