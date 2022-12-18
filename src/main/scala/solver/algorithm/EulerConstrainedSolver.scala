package xyz.mojashi
package solver.algorithm

import automaton.ParikhAutomaton

import xyz.mojashi.formula
import xyz.mojashi.formula.{Add, Constant, EQ, Var}
import xyz.mojashi.solver.BaseSolver

trait EulerConstrainedSolver[In, State, Label, Value, InnerValue] extends BaseSolver[In, State, Label, Value, InnerValue] {

  def initEulerConstraint(implicit m: Numeric[InnerValue]) = {
    pa.voa.states.map(s => {
      addInnerConstraint(convPredicate(
        EQ(
          pa.voa.sourceFrom(s)
            .map(t => Var[InnerVarName, InnerValue](getInnerVariableForNumEdgeUsed(t.id).name))
            .fold(Constant[InnerVarName, InnerValue](m.zero))((sum, t) => Add[InnerVarName, InnerValue](sum, t)),

          Add(
            pa.voa.targetTo(s)
              .map(t => Var[InnerVarName, InnerValue](getInnerVariableForNumEdgeUsed(t.id).name))
              .fold(Constant[InnerVarName, InnerValue](m.zero))((sum, t) => Add[InnerVarName, InnerValue](sum, t)),

            s match {
              case pa.voa.fin => Constant[InnerVarName, InnerValue](m.negate(m.one))
              case pa.voa.start => Constant[InnerVarName, InnerValue](m.one)
              case _ => Constant[InnerVarName, InnerValue](m.zero)
            }
          ),
        )
      ))
    })
  }
}
