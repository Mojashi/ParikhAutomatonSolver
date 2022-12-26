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
  largeConstant: Double = 1e6
)(implicit cast: NumericCast[Value, Double])
  extends MIPBasedSolver[In, Label, Value](pa, lpRelaxed = false, ensureConnectivity = true, underlyingSolver = underlyingSolver) {

  override def solve(): Option[Map[EdgeID, Double]] = {
    def rec(): Option[Map[EdgeID, Double]] = {
      Logger("solve result").debug("rec")

      val numEdgeUsedOpt = solveInner
      if (numEdgeUsedOpt.isEmpty) return None
      val numEdgeUsed = numEdgeUsedOpt.get

      val cuts = pa.voa.calcNEUCut(pa.voa.start, numEdgeUsed)

      if (cuts.isEmpty) {
        return Some(numEdgeUsed)
      }

      for ((comp, cut) <- cuts) {
        addInnerAtomConstraint(
          GTEQ(
            Times(
              Constant[InnerVarName, Double](largeConstant),
              cut
                .map(e => Var[InnerVarName, Double](getInnerVariableForNumEdgeUsed(e).name))
                .fold[Expression[InnerVarName, Double]](Constant[InnerVarName, Double](0))((sum, c) => Add(sum, c)),
            ),
            comp.flatMap(s =>
              pa.voa.sourceFrom(s).map(t =>
                Var[InnerVarName, Double](getInnerVariableForNumEdgeUsed(t.id).name),
              )
            )
              .fold(Constant[InnerVarName, Double](0))((sum, c) => Add(sum, c))
          )
        )
      }

      rec()
    }


    rec()
  }
}
