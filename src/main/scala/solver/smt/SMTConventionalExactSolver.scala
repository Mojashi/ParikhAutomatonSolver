package com.github.Mojashi
package solver.smt

import automaton.ParikhAutomaton

import com.typesafe.scalalogging.Logger
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers
import formula.{Add, And, Constant, EQ, GTEQ, LTEQ, Or, Var}
import graph.EdgeID

import scala.util.Using

class SMTConventionalExactSolver[In, State, Label]
(
  pa: ParikhAutomaton[In, State, Label, Int],
  underlyingSolver: SolverContextFactory.Solvers = Solvers.SMTINTERPOL,
) extends SMTBasedSolver[In, State, Label] (
  pa, underlyingSolver
) {

  override def solve(): Option[Map[EdgeID, Double]] = {
    if (prover.isUnsat)
      return None
    val ret = Using(prover.getModel()) { model =>
      variableRegistry.foreach { case (key, v) =>
        Logger("solve result").debug (s"$key: ${model.evaluate(v.v)}")
      }


      pa.voa.transitions.map(t => {
        val c = model.evaluate(getInnerVariableForNumEdgeUsed(t.id).v)
        if (c.doubleValue().round != c.intValueExact()) {
          throw new Exception("too big")
        }
        (t.id, c.doubleValue())
      }).toMap
    }


    ret.toEither match {
      case Left(e) =>
        Logger("solve result").info(e.toString)
        Some(Map())
      case Right(r) =>
        Logger("solve result").info(r.toString())
        Some(r)
    }
  }

  def getInnerVariableForDistance(s: State): InnerVarWithName = {
    getInnerVariable(s"DISTANCE{$s}")
  }

  def initConnectivityConstraint = {
    {
      pa.voa.states.foreach(s =>
        addInnerConstraint(convPredicate(
        s match {
          case pa.voa.start =>
              EQ(Var[InnerVarName, Int](getInnerVariableForDistance(s).name), Constant[InnerVarName, Int](0))
          case s =>
              Or(Seq(
                // when the state s is unreachable
                And(Seq(
                  EQ(pa.voa.sourceFrom(s)
                    .map(t => Var[InnerVarName, Int](getInnerVariableForNumEdgeUsed(t.id).name))
                    .fold(Constant[InnerVarName, Int](0))((sum, t) => Add[InnerVarName, Int](sum, t)),
                    Constant[InnerVarName, Int](0)
                  ),
                  EQ(Var[InnerVarName, Int](getInnerVariableForDistance(s).name), Constant[InnerVarName, Int](-1)),
                )),
                // when the state s is reachable
                And(Seq(
                  GTEQ(pa.voa.sourceFrom(s)
                    .map(t => Var[InnerVarName, Int](getInnerVariableForNumEdgeUsed(t.id).name))
                    .fold(Constant[InnerVarName, Int](0))((sum, t) => Add[InnerVarName, Int](sum, t)),
                    Constant[InnerVarName, Int](1)
                  ),
                  Or(
                    pa.voa.targetTo(s).map(t =>
                      And(Seq(
                        GTEQ(Var[InnerVarName, Int](getInnerVariableForNumEdgeUsed(t.id).name), Constant[InnerVarName, Int](1)),
                        GTEQ(Var[InnerVarName, Int](getInnerVariableForDistance(t.from).name), Constant[InnerVarName, Int](0)),
                        EQ(
                          Add(Var[InnerVarName, Int](getInnerVariableForDistance(t.from).name), Constant[InnerVarName, Int](1)),
                          Var[InnerVarName, Int](getInnerVariableForDistance(t.to).name),
                        ),
                      ))
                    ))
                ))
              ))
          }
        ))
      )
    }
  }

  initConnectivityConstraint
}
