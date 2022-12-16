package xyz.mojashi

import xyz.mojashi.graph.UnionFind
import scala.collection.mutable

package object graph {
  def getEulerTrail[In, State]
  (
    g: NFA[In, State],
    neu: Map[Edge[State]#EdgeID, Int]
  ): Seq[Edge[State]] = {
    val curNEU = mutable.Map.empty[Edge[State]#EdgeID, Int]

    val trail = mutable.ListBuffer[Edge[State]#EdgeID]()

    def dfs(v: State, lastEdge: Option[Edge[State]#EdgeID]): Unit = {
      for(e <- g.sourceFrom(v) if curNEU.getOrElse(e.id, 0) < neu.getOrElse(e.id, 0)) {
        curNEU(e.id)+=1
        dfs(e.to, Some(e.id))
      }

      lastEdge.foreach(trail.addOne)
    }

    dfs(g.start, None)

    for {
      t <- trail.toSeq
      e <- g.findTransitionByID(t)
    } yield e
  }


  def findConnectedComponent[State]
  (
    g: Graph[State], numEdgeUsed: Map[Edge[State]#EdgeID, Double]
  ): UnionFind[State] = {
    val uf = new UnionFind[State]()

    for {
      (id, c) <- numEdgeUsed if c > 0
      t <- g.findTransitionByID(id)
    } uf.union(t.from, t.to)

    uf
  }
}
