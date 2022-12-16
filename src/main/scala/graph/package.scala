package xyz.mojashi

import xyz.mojashi.automaton.{NFA, Transition}
import xyz.mojashi.graph.{EdgeID, UnionFind}

import scala.collection.mutable

package object graph {
  type EdgeID = Int

  def getEulerTrail[In, State, T <: Transition[Option[In], State]]
  (
    g: NFA[In, State, T],
    neu: Map[EdgeID, Int]
  ): Seq[T] = {
    val curNEU = mutable.Map.empty[EdgeID, Int]

    val trail = mutable.ListBuffer[EdgeID]()

    def dfs(v: State, lastEdge: Option[EdgeID]): Unit = {
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


  def findConnectedComponent[State, T <: Edge[State]]
  (
    g: Graph[State, T], numEdgeUsed: Map[EdgeID, Double]
  ): UnionFind[State] = {
    val uf = new UnionFind[State]()

    for {
      (id, c) <- numEdgeUsed if c > 0
      t <- g.findTransitionByID(id)
    } uf.union(t.from, t.to)

    uf
  }
}
