package xyz.mojashi
package solver.mp

import com.google.ortools.linearsolver.{MPConstraint, MPSolver, MPVariable}

class MIPExactSolver[In, State, Label, Value: Numeric]
(
  pa: ParikhAutomaton[In, State, Label, Value]
)
  extends MIPBasedSolver[In, State, Label, Value](pa) {

  override val IntegerNumEdgeUsed = true

  override def initBaseConstraint: Seq[MPConstraint] = {
    super.initBaseConstraint
    initConnectivityFlowConstraint
  }

  def initConnectivityFlowConstraint = {
    pa.voa.states.map(s => {
      val cons = getMPConstraintForConnectivityFlow(s)

      s match {
        case pa.voa.start => cons.setBounds(-MPSolver.infinity(), 0)
        case _ => cons.setBounds(0, MPSolver.infinity())
      }

      pa.voa.sourceFrom(s).foreach(t =>
        cons.setCoefficient(getMPVariableForConnectivityFlow(t.id), -1)
      )
      pa.voa.targetTo(s).foreach(t => {
        val v = getMPVariableForConnectivityFlow(t.id)
        cons.setCoefficient(v, cons.getCoefficient(v) + 1)
      })

      cons
    })
  }

  def getMPVariableForConnectivityFlow(edgeID: EdgeID): MPVariable = {
    getMPVariable(s"CONNECTIVITY_FLOW{$edgeID}", v => {
      v.setInteger(false)
      v.setLb(0)
    })
  }
  def getMPConstraintForConnectivityFlow(state: State): MPConstraint = {
    getConstraint(s"FLOW_CONSTRAINT{$state}")
  }

  def rec(connected: Set[State], notConnected: Set[State]): Option[Map[EdgeID, Value]] = {
    assert(pa.voa.fin != notConnected)
    assert(pa.voa.fin != connected)
    assert(notConnected.intersect(connected).isEmpty)

    pa.voa.states.foreach(v => {
      if (connected.contains(v))
        getMPConstraintForConnectivityFlow(v).setLb(Math.min(0.01, 0.5 / connected.size))
      else if (v != pa.voa.start)
        getMPConstraintForConnectivityFlow(v).setLb(0)
    })
    pa.voa.transitions.foreach(e => {
      if (notConnected.contains(e.to) || notConnected.contains(e.from))
        getMPVariableForNumEdgeUsed(e.id).setUb(0)
      else
        getMPVariableForNumEdgeUsed(e.id).setUb(MPSolver.infinity())
    })

    val result = mpSolver.solve()

    if (result != MPSolver.ResultStatus.FEASIBLE && result != MPSolver.ResultStatus.OPTIMAL) {
      return None
    }

    val objectiveVal = mpSolver.objective().value()
    val numEdgeUsed: Map[EdgeID, Double] = pa.voa.transitions.map(t =>
      (t.id, getMPVariableForNumEdgeUsed(t.id).solutionValue())
    ).toMap

    val connectedComponents = findConnectedComponent(pa.voa, numEdgeUsed)

    val mainComponent = connectedComponents.find(pnfa.start)
    if (!connected.forall(s => connectedComponents.find(s) == mainComponent)) {
      println("dasaadsasd")
      assert(false)
    }
    assert(connected.forall(s => connectedComponents.find(s) == mainComponent))
    assert(notConnected.forall(s => connectedComponents.rank(s) == 1))
    val roots = connectedComponents.findRoots.diff(Set(mainComponent)).take(1)

    if (roots.size >= 1) {
      var minObj = MPSolver.infinity()
      var minObjEUC = Map[EdgeID, Double]()
      for (r <- roots) {
        val newAddNonCon = roots.diff(r)
        val newAddCon = r

        val tmp = rec(connected ++ newAddCon, notConnected ++ newAddNonCon)

        if (tmp.isDefined) {
          if (exactLowerBound) {
            if (minObj > tmp.get._1) {
              minObj = Math.min(minObj, tmp.get._1)
              minObjEUC = tmp.get._2
            }
          } else return tmp
        }
      }
      if (!minObjEUC.isEmpty)
        Some((minObj, minObjEUC))
      else
        None
    }
    else {
      println(s"obj: ${objectiveVal}")
      Some(objectiveVal, edgeUseCount)
    }
  }
  override def solve(): Option[Map[EdgeID, Value]] = {

  }

  def solveInput(): Seq[In] = {

  }


  def findConnectedComponent[State](g: Graph[State], numEdgeUsed: Map[Edge[State]#EdgeID, Double]): UnionFind[State] = ???
}
