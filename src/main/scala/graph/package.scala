package com.github.Mojashi

import com.github.Mojashi.automaton.{NFA, Transition}
import com.github.Mojashi.graph.{EdgeID, UnionFind}

import scala.collection.mutable

package object graph {
  type EdgeID = String
  type StateID = String

  def getEulerTrail[In, T <: Transition[In]]
  (
    g: NFA[In, T],
    neu: Map[EdgeID, Int]
  ): Seq[T] = {
    val curNEU = mutable.Map.empty[EdgeID, Int]

    val trail = mutable.ListBuffer[EdgeID]()

    def dfs(v: StateID, lastEdge: Option[EdgeID]): Unit = {
      for(e <- g.sourceFrom(v) if curNEU.getOrElse(e.id, 0) < neu.getOrElse(e.id, 0)) {
        curNEU(e.id) = curNEU.getOrElse(e.id, 0) + 1
        dfs(e.to, Some(e.id))
      }

      lastEdge.foreach(trail.addOne)
    }

    dfs(g.start, None)

    assert(neu.forall{case (id, c) => curNEU.getOrElse(id, 0) == c})

    (for {
      t <- trail.toSeq
      e <- g.findTransitionByID(t)
    } yield e).reverse
  }


  def findConnectedComponent[T <: Edge]
  (
    g: Graph[T], numEdgeUsed: Map[EdgeID, Double]
  ): UnionFind[StateID] = {
    val uf = new UnionFind[StateID]()

    for {
      (id, c) <- numEdgeUsed if c > 0
      t <- g.findTransitionByID(id)
    } uf.union(t.from, t.to)

    uf
  }
}
