package com.github.Mojashi
package solver.common

import automaton.ParikhAutomaton

import com.github.Mojashi.formula
import com.github.Mojashi.formula.{Add, Constant, EQ, Var}
import com.github.Mojashi.solver.BaseSolver

import scala.+:

trait EulerConstrainedSolver[In, Label, Value, InnerValue] extends BaseSolver[In, Label, Value, InnerValue] {

  def initEulerConstraint(implicit m: Numeric[InnerValue]) = {
    (pa.voa.states ++ Set(pa.voa.start, pa.voa.fin)).map(s => {
      addInnerAtomConstraint(
        EQ(
          Add(
          pa.voa.sourceFrom(s)
            .map(t => Var[InnerVarName, InnerValue](getInnerVariableForNumEdgeUsed(t.id).name))
          ),

          Add(
            pa.voa.targetTo(s)
              .map(t => Var[InnerVarName, InnerValue](getInnerVariableForNumEdgeUsed(t.id).name)) :+
            (
              s match {
                case pa.voa.fin => Constant[InnerVarName, InnerValue](m.negate(m.one))
                case pa.voa.start => Constant[InnerVarName, InnerValue](m.one)
                case _ => Constant[InnerVarName, InnerValue](m.zero)
              }
            )
          ),
        )
      )
    })
  }
}
