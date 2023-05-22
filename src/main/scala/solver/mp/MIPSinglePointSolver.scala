package com.github.Mojashi
package solver.mp

import com.google.ortools.linearsolver.{MPConstraint, MPSolver, MPVariable}
import com.typesafe.scalalogging.Logger
import automaton.ParikhAutomaton
import com.github.Mojashi.graph
import graph.{EdgeID, StateID}
import solver.common.NumericCast

class MIPSinglePointSolver[In, Label, Value: Numeric]
(
  pa: ParikhAutomaton[In, Label, Value],
  lpRelaxed: Boolean = false,
  ensureOptimumObjective: Boolean = true,
  underlyingSolver: ORToolsMIPSolver = ORToolsMIPSolver.SCIP,
  singleTimeout: Int = 30000,
)(implicit cast: NumericCast[Value, Double])
  extends MIPBasedSolver[In, Label, Value](pa, lpRelaxed, ensureConnectivity = true, underlyingSolver = underlyingSolver, singleTimeout=singleTimeout) {


  def initConnectivityFlowConstraint = {
    pa.voa.states.map(s => {
      val cons = getMPConstraintForConnectivityFlow(s)

      s match {
        case pa.voa.start => cons.setBounds(-MPSolver.infinity(), 0)
        case _ => cons.setBounds(0, MPSolver.infinity())
      }

      pa.voa.sourceFrom(s).foreach(t =>
        cons.setCoefficient(getMPVariableForConnectivityFlow(t.id).v, -1)
      )
      pa.voa.targetTo(s).foreach(t => {
        val v = getMPVariableForConnectivityFlow(t.id).v
        cons.setCoefficient(v, cons.getCoefficient(v) + 1)
      })

      cons
    })

    pa.voa.transitions.foreach(t => {
      val cons = mpSolver.makeConstraint(0, MPSolver.infinity())
      cons.setCoefficient(getInnerVariableForNumEdgeUsed(t.id).v, 1)
      cons.setCoefficient(getMPVariableForConnectivityFlow(t.id).v, -1)
    })
  }

  def getMPVariableForConnectivityFlow(edgeID: EdgeID): InnerVarWithName = {
    getInnerVariable(s"CONNECTIVITY_FLOW{$edgeID}")
  }
  def constraintConnectivityFlowIsPositive = {
    pa.voa.transitions.map(v=>getMPVariableForConnectivityFlow(v.id).v).foreach(v => {
        v.setInteger(false)
        v.setLb(0)
      }
    )
  }

  def getMPConstraintForConnectivityFlow(state: StateID): MPConstraint = {
    getConstraint(s"FLOW_CONSTRAINT{$state}")
  }

  def rec(connected: Set[StateID], notConnected: Set[StateID]): Option[(Double, Map[InnerVarName, Double])] = {
    Logger("rec").debug(s"rec: ${connected.size}")

    assert(pa.voa.fin != notConnected)
    assert(pa.voa.fin != connected)
    assert(notConnected.intersect(connected).isEmpty)

    if(connected.nonEmpty) initConnectivityFlow()
    pa.voa.states.foreach(v => {
      if (connected.contains(v))
        getMPConstraintForConnectivityFlow(v).setLb(Math.min(0.1, 1.0 / connected.size))
      else if (v != pa.voa.start)
        getMPConstraintForConnectivityFlow(v).setLb(0)
    })
    pa.voa.transitions.foreach(e => {
      if (notConnected.contains(e.to) || notConnected.contains(e.from))
        getInnerVariableForNumEdgeUsed(e.id).v.setUb(0)
      else
        getInnerVariableForNumEdgeUsed(e.id).v.setUb(MPSolver.infinity())
    })

    val modelOpt = solveInner
    if(modelOpt.isEmpty) return None
    val objectiveVal = mpSolver.objective().value()
    val model = modelOpt.get

    val neu = pa.voa.transitions.map(t =>
      (t.id, model.getOrElse(getInnerVariableForNumEdgeUsed(t.id).name, 0.0))
    ).toMap
    val connectedComponents = graph.findConnectedComponent(pa.voa, neu)

    val mainComponent = connectedComponents.find(pa.voa.start)
    assert(connected.forall(s => connectedComponents.find(s) == mainComponent))
    assert(notConnected.forall(s => connectedComponents.rank(s) == 1))
    val notConnectedRootOpt = connectedComponents.findRoots.diff(Set(mainComponent)).headOption

    if (notConnectedRootOpt.isDefined) {
      val notConnectedRoot = notConnectedRootOpt.get
      var minObj = MPSolver.infinity()
      var minObjModel = Map[InnerVarName, Double]()

      for ((newConnected, newNotConnected) <- Seq((Set(notConnectedRoot), Set()), (Set(), Set(notConnectedRoot)))) {
        val tmp = rec(connected ++ newConnected, notConnected ++ newNotConnected)

        if (tmp.isDefined) {
          val (objV, neu) = tmp.get
          if(isConstantObjective() || !ensureOptimumObjective) return tmp

          if (minObj > objV) {
            minObj = Math.min(minObj, objV)
            minObjModel = neu
          }
        }
      }

      if (minObj.isFinite)
        Some(minObj, minObjModel)
      else
        None
    } else {
      Some(objectiveVal, model)
    }
  }

  var latestEarnedModel: Option[Map[InnerVarName, Double]] = None
  override def solve(): Option[Map[EdgeID, Double]] = {

    val ret = orHandleSolve(() => rec(Set(), Set()).flatMap(r => Some(r._2)))
    //println(ret)

    latestEarnedModel = ret
    latestEarnedModel.flatMap(model =>
      Some(
        pa.voa.transitions.map(t =>
          (t.id, model.getOrElse(getInnerVariableForNumEdgeUsed(t.id).name, 0.0))
        ).toMap
      )
    )
  }


  var initializedConnectivityFlow = false
  def initConnectivityFlow() = {
    if(!initializedConnectivityFlow) {
      initializedConnectivityFlow = true
      initConnectivityFlowConstraint
      constraintConnectivityFlowIsPositive
    }
  }
}

