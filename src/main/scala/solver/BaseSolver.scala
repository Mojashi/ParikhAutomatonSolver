package xyz.mojashi
package solver

import formula.{Add, And, Constant, EQ, Expression, GTEQ, LTEQ, Or, Predicate, Sub, Times, Var}
import graph.EdgeID

import xyz.mojashi.automaton.ParikhAutomaton
import xyz.mojashi.solver.algorithm.NumericCast

abstract class BaseSolver[In, State, Label, Value: Numeric, InnerValue: Numeric]
(implicit valueM: Numeric[Value], innerM: Numeric[InnerValue], cast: NumericCast[Value, InnerValue])
{
  type InnerVarName = String
  type InnerVar
  type InnerExpr
  type InnerConstraint

  val pa: ParikhAutomaton[In, State, Label, Value]
  val labels = pa.voa.transitions.flatMap(t => t.out.keys).toSet


  case class InnerVarWithName (
    name: InnerVarName,
    v: InnerVar
  )
  def getInnerVariable(name: InnerVarName): InnerVarWithName

  def convPredicate[L](p: Predicate[L, InnerValue]): InnerConstraint

  def convExpr[L](e: Expression[L, InnerValue]): InnerExpr

  def getInnerVariableForNumEdgeUsed(edgeID: EdgeID): InnerVarWithName = {
    getInnerVariable(s"NUM_EDGE_USED{$edgeID}")
  }
  def constraintNumEdgeUsedIsPositive = {
    pa.voa.transitions.foreach(t => addInnerConstraint(convPredicate(
      GTEQ[InnerVarName,InnerValue](
        Var[InnerVarName,InnerValue](getInnerVariableForNumEdgeUsed(t.id).name),
        Constant[InnerVarName,InnerValue](innerM.zero)
      )(innerM)
    )))
  }

  def constraintPAConstraint= {
    addInnerConstraint(convPredicate(convParikhPredicateToInner(pa.constraint)))
  }

  def getInnerVariableForLabel(label: Label): InnerVarWithName = {
    getInnerVariable(s"LABEL{$label}")
  }
  def addInnerConstraint(cons: InnerConstraint)


  def convParikhPredicateToInner(p: Predicate[Label, Value]): Predicate[InnerVarName, InnerValue] = {
    p match {
      case And(ps) => And[InnerVarName,InnerValue](ps.map(convParikhPredicateToInner))
      case Or(ps) => And[InnerVarName,InnerValue](ps.map(convParikhPredicateToInner))
      case GTEQ(left, right) => GTEQ[InnerVarName,InnerValue](convParikhExprToInner(left), convParikhExprToInner(right))(innerM)
      case LTEQ(left, right) => LTEQ[InnerVarName,InnerValue](convParikhExprToInner(left), convParikhExprToInner(right))(innerM)
      case EQ(left, right) => EQ[InnerVarName,InnerValue](convParikhExprToInner(left), convParikhExprToInner(right))(innerM)
    }
  }

  def convParikhExprToInner(p: Expression[Label, Value]): Expression[InnerVarName, InnerValue] = {
    p match {
      case Add(left, right) => Add[InnerVarName,InnerValue](convParikhExprToInner(left), convParikhExprToInner(right))(innerM)
      case Sub(left, right) => Sub[InnerVarName,InnerValue](convParikhExprToInner(left), convParikhExprToInner(right))(innerM)
      case Times(left, right) => Times[InnerVarName,InnerValue](Constant(cast.cast(left.v)), convParikhExprToInner(right))(innerM)
      case Var(v) => Var[InnerVarName,InnerValue](getInnerVariableForLabel(v).name)
      case Constant(v) => Constant(cast.cast(v))
    }
  }

  def addConstraint(constraint: Predicate[Label, Value], constraintID: String): String = {
    addInnerConstraint(convPredicate(convParikhPredicateToInner(constraint)))
    constraintID
  }
}