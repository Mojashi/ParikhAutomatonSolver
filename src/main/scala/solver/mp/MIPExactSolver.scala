package xyz.mojashi
package solver.mp

import com.google.ortools.linearsolver.{MPConstraint, MPSolver, MPVariable}
import com.typesafe.scalalogging.Logger
import xyz.mojashi.automaton.ParikhAutomaton
import xyz.mojashi.graph
import xyz.mojashi.graph.EdgeID


class MIPExactSolver[In, State, Label, Value: Numeric]
(
  pa: ParikhAutomaton[In, State, Label, Value]
)
  extends MIPBasedSolver[In, State, Label, Value](pa) {

  override def IntegerNumEdgeUsed = true

  override def initBaseConstraint: Unit = {
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

    pa.voa.transitions.foreach(t => {
      val cons = mpSolver.makeConstraint(0, MPSolver.infinity())
      cons.setCoefficient(getMPVariableForNumEdgeUsed(t.id), 1)
      cons.setCoefficient(getMPVariableForConnectivityFlow(t.id), -1)
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

  def rec(connected: Set[State], notConnected: Set[State]): Option[(Double, Map[EdgeID, Double])] = {
    Logger("rec").debug(s"rec: ${connected.size}")

    assert(pa.voa.fin != notConnected)
    assert(pa.voa.fin != connected)
    assert(notConnected.intersect(connected).isEmpty)

    pa.voa.states.foreach(v => {
      if (connected.contains(v))
        getMPConstraintForConnectivityFlow(v).setLb(Math.min(0.1, 1.0 / connected.size))
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
    Logger("result").debug(result.toString)
    if (result != MPSolver.ResultStatus.FEASIBLE && result != MPSolver.ResultStatus.OPTIMAL) {
      return None
    }

    val objectiveVal = mpSolver.objective().value()
    val numEdgeUsed: Map[EdgeID, Double] = pa.voa.transitions.map(t =>
      (t.id, getMPVariableForNumEdgeUsed(t.id).solutionValue())
    ).toMap

    val connectedComponents = graph.findConnectedComponent(pa.voa, numEdgeUsed)

    val mainComponent = connectedComponents.find(pa.voa.start)
    assert(connected.forall(s => connectedComponents.find(s) == mainComponent))
    assert(notConnected.forall(s => connectedComponents.rank(s) == 1))
    val notConnectedRootOpt = connectedComponents.findRoots.diff(Set(mainComponent)).headOption

    if (notConnectedRootOpt.isDefined) {
      val notConnectedRoot = notConnectedRootOpt.get
      var minObj = MPSolver.infinity()
      var minObjNEU = Map[EdgeID, Double]()

      for ((newConnected, newNotConnected) <- Seq((Set(notConnectedRoot), Set()), (Set(), Set(notConnectedRoot)))) {
        val tmp = rec(connected ++ newConnected, notConnected ++ newNotConnected)

        if (tmp.isDefined) {
          val (objV, neu) = tmp.get
          if(isConstantObjective()) return tmp

          if (minObj > objV) {
            minObj = Math.min(minObj, objV)
            minObjNEU = neu
          }
        } else return tmp
      }

      if (minObj.isFinite)
        Some((minObj, minObjNEU))
      else
        None
    } else {
      Some(objectiveVal, numEdgeUsed)
    }
  }
  override def solve(): Option[Map[EdgeID, Double]] = {
    val ret = rec(Set(), Set())
    println(ret)
    ret.flatMap(r => Some(r._2))
  }

  def solveInput(): Option[Seq[In]] = {
    solve().flatMap(neu => Some(
      graph.getEulerTrail(
        pa.voa,
        neu.map{case (e,c) =>
          (e, Math.round(c).toInt)
        }).flatMap(t =>
          pa.voa.findTransitionByID(t.id).get.in
        )
    ))
  }

}

