package com.github.Mojashi
package solver.mp

import automaton.ParikhAutomaton
import graph.EdgeID

import com.google.ortools.linearsolver.MPSolver
import com.typesafe.scalalogging.Logger
import com.github.Mojashi.solver.algorithm.NumericCast

class LPRelaxedSolver[In, Label, Value: Numeric]
(
  pa: ParikhAutomaton[In, Label, Value]
)(implicit cast: NumericCast[Value, Double])
  extends MIPBasedSolver[In, Label, Value](pa, true) {
  override def solve(): Option[Map[EdgeID, Double]] = {
    val result = mpSolver.solve()
    Logger("result").debug(result.toString)
    if (result != MPSolver.ResultStatus.FEASIBLE && result != MPSolver.ResultStatus.OPTIMAL) {
      return None
    }

    val objectiveVal = mpSolver.objective().value()
    Logger("objective").info(s"objectiveVal: $objectiveVal")
    val numEdgeUsed: Map[EdgeID, Double] = pa.voa.transitions.map(t =>
      (t.id, getInnerVariableForNumEdgeUsed(t.id).v.solutionValue())
    ).toMap

    Some(numEdgeUsed)
  }

}
