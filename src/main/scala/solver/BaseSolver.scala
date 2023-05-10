package com.github.Mojashi
package solver

import formula.{Add, And, AtomPredicate, Constant, EQ, Expression, GTEQ, LTEQ, Or, Predicate, Sub, Times, Var}
import graph.EdgeID

import com.github.Mojashi.automaton.ParikhAutomaton
import com.github.Mojashi.solver.common.NumericCast

import scala.collection.mutable

abstract class BaseSolver[In, Label, Value: Numeric, InnerValue: Numeric]
(implicit valueM: Numeric[Value], innerM: Numeric[InnerValue], cast: NumericCast[Value, InnerValue])
{
  type InnerVarName = String
  type InnerVar
  type InnerExpr
  type InnerConstraint

  val pa: ParikhAutomaton[In, Label, Value]
  val labels = pa.voa.transitions.flatMap(t => t.out.getOrElse(Map()).keys).toSet

  case class InnerVarWithName (
    name: InnerVarName,
    v: InnerVar
  )
  def getInnerVariable(name: InnerVarName): InnerVarWithName

  def addInnerConstraint(p: Predicate[InnerVarName, InnerValue])

  def addInnerAtomConstraint(p: AtomPredicate[InnerVarName, InnerValue]): Any


  def getInnerVariableForNumEdgeUsed(edgeID: EdgeID): InnerVarWithName = {
    getInnerVariable(s"NUM_EDGE_USED{$edgeID}")
  }
  def constraintNumEdgeUsedIsPositive = {
    pa.voa.transitions.foreach(t => addInnerAtomConstraint(
      GTEQ[InnerVarName,InnerValue](
        Var[InnerVarName,InnerValue](getInnerVariableForNumEdgeUsed(t.id).name),
        Constant[InnerVarName,InnerValue](innerM.zero)
      )(innerM)
    ))
  }

  def constraintPAConstraint= {
    addInnerConstraint(convParikhPredicateToInner(pa.constraint))
  }

  def getInnerVariableForLabel(label: Label): InnerVarWithName = {
    getInnerVariable(s"LABEL{$label}")
  }

  def convParikhPredicateToInner(p: Predicate[Label, Value]): Predicate[InnerVarName, InnerValue] = {
    p match {
      case And(ps) => And[InnerVarName,InnerValue](ps.map(convParikhPredicateToInner))
      case Or(ps) => Or[InnerVarName,InnerValue](ps.map(convParikhPredicateToInner))
      case p: AtomPredicate[Label,Value] => convParikhAtomPredicateToInner(p)
    }
  }

  def convParikhAtomPredicateToInner(p: AtomPredicate[Label, Value]): AtomPredicate[InnerVarName, InnerValue] = {
    p match {
      case GTEQ(left, right) => GTEQ[InnerVarName, InnerValue](convParikhExprToInner(left), convParikhExprToInner(right))(innerM)
      case LTEQ(left, right) => LTEQ[InnerVarName, InnerValue](convParikhExprToInner(left), convParikhExprToInner(right))(innerM)
      case EQ(left, right) => EQ[InnerVarName, InnerValue](convParikhExprToInner(left), convParikhExprToInner(right))(innerM)
    }
  }

  def convParikhExprToInner(p: Expression[Label, Value]): Expression[InnerVarName, InnerValue] = {
    p match {
      case Add(terms) => Add[InnerVarName,InnerValue](terms.map(convParikhExprToInner))(innerM)
      case Sub(left, right) => Sub[InnerVarName,InnerValue](convParikhExprToInner(left), convParikhExprToInner(right))(innerM)
      case Times(left, right) => Times[InnerVarName,InnerValue](Constant(cast.cast(left.v)), convParikhExprToInner(right))(innerM)
      case Var(v) => Var[InnerVarName,InnerValue](getInnerVariableForLabel(v).name)
      case Constant(v) => Constant(cast.cast(v))
    }
  }

}