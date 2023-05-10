package com.github.Mojashi
package solver.z3smt

import solver.{BaseSolver, ParikhAutomatonSolver}

import automaton.ParikhAutomaton
import solver.common.{CalcParikhConstrainedSolver, EulerConstrainedSolver}

import com.github.Mojashi.formula.{Add, And, AtomPredicate, Constant, EQ, Expression, GTEQ, LTEQ, Or, Predicate, Sub, Times, Var}
import com.github.Mojashi.graph.EdgeID
import com.github.Mojashi.solver.common.Implicits.IntIntNumericCast
import com.microsoft.z3
import com.microsoft.z3.Context

import java.util
import scala.collection.mutable
import collection.JavaConverters._

abstract class Z3BasedSolver[In, Label]
(
  override val pa: ParikhAutomaton[In, Label, Int],
  timeout: Int
)
  extends BaseSolver[In, Label,Int, Int]
    with ParikhAutomatonSolver[In, Label, Int]
    with AutoCloseable
    with EulerConstrainedSolver[In, Label,Int, Int]
    with CalcParikhConstrainedSolver[In, Label,Int, Int] {

  override type InnerExpr = z3.Expr[z3.IntSort]
  override type InnerVar = z3.IntExpr
  override type InnerConstraint = z3.BoolExpr


  protected val context = new z3.Context()
  protected val solver = context.mkSolver()

  val p = context.mkParams
  p.add("timeout", timeout)
  solver.setParameters(p)

  protected val variableRegistry = mutable.Map.empty[String, InnerVarWithName]

  override def getInnerVariable(name: String) = {
    variableRegistry.getOrElseUpdate(
      name, InnerVarWithName(name=name, v=context.mkIntConst(name))
    )
  }

  override def setObjective(minimize: Expression[Either[Label, EdgeID], Int]): Unit = {
    throw new Exception("You cannot set objective with SMT")
  }

  def addInnerConstraint(constraint: InnerConstraint) = {
    solver.add(constraint)
  }

  override def addInnerConstraint(p: Predicate[InnerVarName, Int]) = {
    addInnerConstraint(convPredicate(p))
  }

  def convPredicate[L](p: Predicate[L, Int]): InnerConstraint = {
    p match {
      case GTEQ(left, right) => context.mkGe(convExpr(left), convExpr(right))
      case LTEQ(left, right) => context.mkLe(convExpr(left), convExpr(right))
      case EQ(left, right) => context.mkEq(convExpr(left), convExpr(right))
      case And(ps) => context.mkAnd(ps.map(p=>convPredicate(p)): _*)
      case Or(ps) => context.mkOr(ps.map(p=>convPredicate(p)): _*)
    }
  }

  def convExpr[L](e: Expression[L, Int]): InnerExpr = {
    e match {
      case Add(terms) => context.mkAdd(context.mkInt(0) +: terms.map(convExpr): _*)
      case Sub(left, right) => context.mkSub(convExpr(left), convExpr(right))
      case Times(constant, t) => context.mkMul(convExpr(constant), convExpr(t))
      case Constant(v) => context.mkInt(v)
      case Var(v) => getInnerVariable(v.toString).v
    }
  }

  override def removeConstraint(constraintID: ConstraintID): Unit = {
    throw new Exception("You cannot remove any constraint with SMT")
  }

  override def close(): Unit = {
    context.close()
  }


  override def addConstraint(constraint: Predicate[Label, Int], constraintID: String): String = {
    addInnerConstraint(convParikhPredicateToInner(constraint))
    constraintID
  }

  constraintNumEdgeUsedIsPositive
  initEulerConstraint
  constraintPAConstraint
  initCalcParikhImageConstraint

  override def addInnerAtomConstraint(p: AtomPredicate[InnerVarName, Int]) = {
    addInnerConstraint(p)
  }
}
