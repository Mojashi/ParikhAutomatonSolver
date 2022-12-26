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
  override def solve(): Option[Map[EdgeID, Double]] = {
    def rec(): Option[Map[EdgeID, Double]] = {
      Logger("solve result").debug("rec")

      if (prover.isUnsat)
        return None

      val neuOption = Using(prover.getModel) { model =>
        Logger("solve result").whenDebugEnabled (
          variableRegistry.foreach { case (key, v) =>
          //  Logger("solve result").debug(s"$key: ${model.evaluate(v.v)}")
          }
        )


        pa.voa.transitions.map(t => {
          val c = model.evaluate(getInnerVariableForNumEdgeUsed(t.id).v)
          if (c.doubleValue().round != c.intValueExact()) {
            throw new Exception("too big")
          }
          (t.id, c.doubleValue())
        }).toMap
      }.toEither

      neuOption match {
        case Left(e) =>
          Logger("solve result").info(e.toString)
          Some(Map())
        case Right(neu) =>
          val cuts = pa.voa.calcNEUCut(pa.voa.start, neu)

          if (cuts.isEmpty) {
            return Some(neu)
          }

          for ((comp, cut) <- cuts) {
            addInnerConstraint(
              Or(Seq(
                And(Seq(
                  EQ(
                    cut
                      .map(e => Var[InnerVarName, Int](getInnerVariableForNumEdgeUsed(e).name))
                      .fold[Expression[InnerVarName, Int]](Constant[InnerVarName, Int](0))((sum, c) => Add(sum, c)),
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
                  cut
                    .map(e => Var[InnerVarName, Int](getInnerVariableForNumEdgeUsed(e).name))
                    .fold(Constant[InnerVarName, Int](0))((sum, c) => Add(sum, c)),
                  Constant[InnerVarName, Int](1)
                ),
              ))
            )
          }

          rec()

      }
    }


    rec()
  }
}
