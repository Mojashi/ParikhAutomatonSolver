package com.github.Mojashi
package solver.smt

import automaton.ParikhAutomaton
import formula._
import graph.{EdgeID, StateID}

import com.typesafe.scalalogging.Logger
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers
import solver.utils.CalcNEUCut.CalcNEUCut
import scala.util.Using

class SMTCutExactSolver[In, Label]
(
  pa: ParikhAutomaton[In, Label, Int],
  underlyingSolver: SolverContextFactory.Solvers = Solvers.SMTINTERPOL,
) extends SMTBasedSolver[In, Label] (
  pa, underlyingSolver
) {

  var latestEarnedModel: Option[Map[InnerVarName, Double]] = None


  override def solve(): Option[Map[EdgeID, Double]] = {
    def rec(): Option[Map[InnerVarName, Double]] = {
      Logger("solve result").debug("rec")

      if (prover.isUnsat)
        return None

      val modelEither = Using(prover.getModel()) { model =>
        variableRegistry.map(v => {
          val c = model.evaluate(v._2.v)
          if (c.doubleValue().round != c.intValueExact()) {
            throw new Exception("too big")
          }
          (v._1, c.doubleValue())
        }).toMap
      }.toEither

      modelEither match {
        case Left(e) =>
          Logger("solve result").info(e.toString)
          Some(Map())
        case Right(model) =>
          val cuts = pa.voa.calcNEUCut(pa.voa.start, pa.voa.transitions.map(t =>
            (t.id, model.getOrElse(getInnerVariableForNumEdgeUsed(t.id).name, 0.0))
          ).toMap)

          if (cuts.isEmpty) {
            return Some(model)
          }

          for ((comp, cut) <- cuts) {
            addInnerConstraint(
              Or(Seq(
                And(Seq(
                  EQ(
                    Add(cut.map(e => Var[InnerVarName, Int](getInnerVariableForNumEdgeUsed(e).name))),
                    Constant[InnerVarName, Int](0),
                  )) ++
                  comp.flatMap(s =>
                    pa.voa.sourceFrom(s).map(t =>
                      EQ (
                        Var[InnerVarName, Int](getInnerVariableForNumEdgeUsed(t.id).name),
                        Constant[InnerVarName, Int](0)
                      )
                    )
                  ),
                ),
                GTEQ(
                  Add(cut.map(e => Var[InnerVarName, Int](getInnerVariableForNumEdgeUsed(e).name))),
                  Constant[InnerVarName, Int](1)
                ),
              ))
            )
          }

          rec()

      }
    }


    latestEarnedModel = rec()

    latestEarnedModel.flatMap(model => Some(
      pa.voa.transitions.map(t =>
        (t.id, model.getOrElse(getInnerVariableForNumEdgeUsed(t.id).name, 0.0))
      ).toMap
    ))
  }
}
