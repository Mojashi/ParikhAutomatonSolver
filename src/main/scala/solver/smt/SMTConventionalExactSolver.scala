package com.github.Mojashi
package solver.smt

import automaton.ParikhAutomaton

import com.typesafe.scalalogging.Logger
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers
import formula.{Add, And, Constant, EQ, GTEQ, LTEQ, Or, Var}
import graph.{EdgeID, StateID}

import scala.util.Using

class SMTConventionalExactSolver[In, Label]
(
  pa: ParikhAutomaton[In, Label, Int],
  underlyingSolver: SolverContextFactory.Solvers = Solvers.SMTINTERPOL,
  timeout: Int = 30000,
) extends SMTBasedSolver[In, Label] (
  pa, underlyingSolver
) {


  var latestEarnedModel: Option[Map[InnerVarName, Double]] = None

  override def solve(): Option[Map[EdgeID, Double]] = {
    var finished = false
    if (timeout > 0) {
      new Thread() {
        override def run() = {
          Thread.sleep(timeout)

          finished.synchronized {
            if (!finished) {
              Logger("SMTConventionalExactSolver").info("timeout! shutdown requested")
              shutdown.requestShutdown("timeout")
            }
          }
        }
      }.start()
    }

    val unsat = prover.isUnsat
    finished.synchronized {
      finished = true
    }
    if (unsat) {
      latestEarnedModel = None
      return None
    }

    val ret = Using(prover.getModel()) { model =>
      variableRegistry.map(v =>{
        val c = model.evaluate(v._2.v)
        if (c.doubleValue().round != c.intValueExact()) {
          throw new Exception("too big")
        }
        (v._1, c.doubleValue())
      }).toMap
    }

    latestEarnedModel = ret.toEither match {
      case Left(e) =>
        Logger("solve result").info(e.toString)
        Some(Map())
      case Right(r) =>
        Logger("solve result").info(r.toString())
        Some(r)
    }

    latestEarnedModel.flatMap(model => Some(
      pa.voa.transitions.map(t =>
        (t.id, model.getOrElse(getInnerVariableForNumEdgeUsed(t.id).name, 0.0))
      ).toMap
    ))
  }

  def getInnerVariableForDistance(s: StateID): InnerVarWithName = {
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
                  EQ(
                    Add(
                      pa.voa.sourceFrom(s)
                        .map(t => Var[InnerVarName, Int](getInnerVariableForNumEdgeUsed(t.id).name))
                    ),
                    Constant[InnerVarName, Int](0)
                  ),
                  EQ(Var[InnerVarName, Int](getInnerVariableForDistance(s).name), Constant[InnerVarName, Int](-1)),
                )),
                // when the state s is reachable
                And(Seq(
                  GTEQ(
                    Add(
                      pa.voa.sourceFrom(s)
                        .map(t => Var[InnerVarName, Int](getInnerVariableForNumEdgeUsed(t.id).name))),
                    Constant[InnerVarName, Int](1)
                  ),
                  Or(
                    pa.voa.targetTo(s).map(t =>
                      And(Seq(
                        GTEQ(Var[InnerVarName, Int](getInnerVariableForNumEdgeUsed(t.id).name), Constant[InnerVarName, Int](1)),
                        GTEQ(Var[InnerVarName, Int](getInnerVariableForDistance(t.from).name), Constant[InnerVarName, Int](0)),
                        EQ(
                          Add(Seq(Var[InnerVarName, Int](getInnerVariableForDistance(t.from).name), Constant[InnerVarName, Int](1))),
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
