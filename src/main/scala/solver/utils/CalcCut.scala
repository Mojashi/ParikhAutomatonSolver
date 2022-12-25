package com.github.Mojashi
package solver.utils

import graph.{Edge, EdgeID, Graph, StateID, findConnectedComponent}

import com.google.ortools.graph.MaxFlow

import scala.collection.mutable

object CalcNEUCut {
  implicit class CalcNEUCut
  (
    g: Graph[Edge]
  ) {
    // foreach connected components, return (vertices in connected components, minimum cut edges)
    def calcNEUCut(start: StateID, neu: Map[EdgeID, Double]): Seq[(Seq[StateID], Seq[EdgeID])] = {
      val uf = findConnectedComponent(g, neu)
      val startComponent = uf.find(start)

      val components = uf.findRoots.diff(Set(startComponent))

      val vertexIdx: Map[StateID, Int] = g.states.zipWithIndex.toMap

      val mf = new MaxFlow()

      g.transitions.foreach(t => {
        val fc = uf.find(t.from)
        val tc = uf.find(t.to)
        mf.addArcWithCapacity(vertexIdx(t.from), vertexIdx(t.to),
          if (fc == tc) Long.MaxValue / 10
          else 1
        )
      })
      val edgeIdToIdx: Map[EdgeID, Int] = g.transitions.zipWithIndex.map{ case (e, idx) => (e.id, idx) }.toMap

      components.toSeq.map(target => {
        assert(mf.solve(vertexIdx(start), vertexIdx(target)) == MaxFlow.Status.OPTIMAL)
        val cut = {
          val reachable = mutable.HashSet[StateID]()
          val que = mutable.Queue[StateID](start)
          while (que.nonEmpty) {
            val s = que.dequeue()
            if (!reachable.contains(s)) {
              reachable.add(s)
              que.addAll(g.sourceFrom(s).filter(e => mf.getFlow(edgeIdToIdx(e.id)) < mf.getCapacity(edgeIdToIdx(e.id))).filter(e => !reachable.contains(e.to)).map(_.to))
              que.addAll(g.targetTo(s).filter(e => mf.getFlow(edgeIdToIdx(e.id)) > 0).filter(e => !reachable.contains(e.from)).map(_.from))
            }
          }

          g.transitions.filter(e => {
            mf.getCapacity(edgeIdToIdx(e.id)) > 0 && reachable.contains(e.from) && !reachable.contains(e.to)
          }).map(e=>e.id)
        }
        assert(cut.size == mf.getOptimalFlow)

        val vs = g.states.filter(s => uf.find(s) == target)
        (vs, cut)
      })
    }
  }
}
