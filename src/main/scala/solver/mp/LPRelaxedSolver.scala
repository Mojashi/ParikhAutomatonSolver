package xyz.mojashi
package solver.mp

import automaton.ParikhAutomaton
import graph.EdgeID

import com.google.ortools.linearsolver.MPSolver
import com.typesafe.scalalogging.Logger

class LPRelaxedSolver[In, State, Label, Value: Numeric]
(
  pa: ParikhAutomaton[In, State, Label, Value]
)
  extends MIPBasedSolver[In, State, Label, Value](pa, true) {
  override def solve(): Option[Map[EdgeID, Double]] = {
    val result = mpSolver.solve()
    Logger("result").debug(result.toString)
    if (result != MPSolver.ResultStatus.FEASIBLE && result != MPSolver.ResultStatus.OPTIMAL) {
      return None
    }

    val objectiveVal = mpSolver.objective().value()
    Logger("objective").info(s"objectiveVal: $objectiveVal")
    val numEdgeUsed: Map[EdgeID, Double] = pa.voa.transitions.map(t =>
      (t.id, getMPVariableForNumEdgeUsed(t.id).solutionValue())
    ).toMap

    Some(numEdgeUsed)
  }

}
