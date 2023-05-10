package com.github.Mojashi
package solver.z3smt

import automaton.ParikhAutomaton
import formula._
import graph.{EdgeID, StateID}

import scala.util.Using
import com.microsoft.z3
import com.microsoft.z3.IntNum
class Z3ConventionalExactSolver[In, Label]
(
  pa: ParikhAutomaton[In, Label, Int],
  timeout: Int = 30000,
) extends Z3BasedSolver[In, Label] (
  pa, timeout
) {


  var latestEarnedModel: Option[Map[InnerVarName, Double]] = None

  override def solve(): Option[Map[EdgeID, Double]] = {
    val result = solver.check()
    println(result)

    if (result == z3.Status.UNSATISFIABLE) {
      latestEarnedModel = None
      return None
    }
    if(result == z3.Status.UNKNOWN) {
      throw new Exception("timeout")
    }

    val model = solver.getModel()
    latestEarnedModel = Some(variableRegistry.map(v =>{
      val c = model.evaluate(v._2.v, true).asInstanceOf[IntNum].getInt
      (v._1, c.doubleValue())
    }).toMap)

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
