package com.github.Mojashi
package solver.mp

import automaton.ParikhAutomaton
import formula._
import graph.EdgeID
import solver.utils.CalcNEUCut.CalcNEUCut

import com.github.Mojashi.solver.common.NumericCast
import com.typesafe.scalalogging.Logger
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers

import scala.util.Using

class MIPCutWithLargeMExactSolver[In, Label, Value: Numeric]
(
  pa: ParikhAutomaton[In, Label, Value],
  underlyingSolver: ORToolsMIPSolver = ORToolsMIPSolver.SCIP,
  largeConstant: Double = 1e6,
  singleTimeout: Int = 30000,
)(implicit cast: NumericCast[Value, Double])
  extends MIPBasedSolver[In, Label, Value](pa, lpRelaxed = false, ensureConnectivity = true, underlyingSolver = underlyingSolver, singleTimeout=singleTimeout) {


  var latestEarnedModel: Option[Map[InnerVarName, Double]] = None
  override def solve(): Option[Map[EdgeID, Double]] = {
    def rec(): Option[Map[InnerVarName, Double]] = {
      Logger("solve result").debug("rec")


      val modelOpt = solveInner
      if (modelOpt.isEmpty) return None
      val model = modelOpt.get

      val numEdgeUsed = pa.voa.transitions.map(t =>
        (t.id, model.getOrElse(getInnerVariableForNumEdgeUsed(t.id).name, 0.0))
      ).toMap

      val cuts = pa.voa.calcNEUCut(pa.voa.start, numEdgeUsed)

      if (cuts.isEmpty) {
        return Some(model)
      }

      for ((comp, cut) <- cuts) {
        addInnerAtomConstraint(
          GTEQ(
            Times(
              Constant[InnerVarName, Double](largeConstant),
              Add(cut
                .map(e => Var[InnerVarName, Double](getInnerVariableForNumEdgeUsed(e).name))
              )
            ),
            Add(comp.flatMap(s =>
              pa.voa.sourceFrom(s).map(t =>
                Var[InnerVarName, Double](getInnerVariableForNumEdgeUsed(t.id).name),
              )
            ))
          )
        )
      }

      rec()
    }


    latestEarnedModel = rec()
    latestEarnedModel.flatMap(model =>
      Some(pa.voa.transitions.map(t =>
        (t.id, model.getOrElse(getInnerVariableForNumEdgeUsed(t.id).name, 0.0))
      ).toMap)
    )
  }
}
