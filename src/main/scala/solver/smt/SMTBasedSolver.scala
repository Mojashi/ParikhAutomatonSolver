package com.github.Mojashi
package solver.smt

import solver.{BaseSolver, ParikhAutomatonSolver}

import org.sosy_lab.common.ShutdownManager
import org.sosy_lab.common.configuration.Configuration
import org.sosy_lab.common.log.BasicLogManager
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers
import org.sosy_lab.java_smt.api.{BooleanFormula, NumeralFormula}
import org.sosy_lab.java_smt.api.SolverContext.ProverOptions
import automaton.ParikhAutomaton
import solver.algorithm.{CalcParikhConstrainedSolver, EulerConstrainedSolver}

import com.github.Mojashi.formula.{Add, And, AtomPredicate, Constant, EQ, Expression, GTEQ, LTEQ, Or, Predicate, Sub, Times, Var}
import com.github.Mojashi.solver.algorithm.Implicits.IntIntNumericCast

import java.util
import scala.collection.mutable
import collection.JavaConverters._

abstract class SMTBasedSolver[In, Label]
(
  override val pa: ParikhAutomaton[In, Label, Int],
  val underlyingSolver: SolverContextFactory.Solvers = Solvers.SMTINTERPOL,
)
  extends BaseSolver[In, Label,Int, Int]
  with ParikhAutomatonSolver[In, Label, Int]
  with AutoCloseable
  with EulerConstrainedSolver[In, Label,Int, Int]
  with CalcParikhConstrainedSolver[In, Label,Int, Int] {

  override type InnerExpr = NumeralFormula.IntegerFormula
  override type InnerVar = NumeralFormula.IntegerFormula
  override type InnerConstraint = BooleanFormula


  private val config = Configuration.defaultConfiguration()
  private val logger = BasicLogManager.create(config)
  private val shutdown = ShutdownManager.create
  private val context = SolverContextFactory.createSolverContext(config, logger, shutdown.getNotifier, underlyingSolver)
  protected val imgr = context.getFormulaManager.getIntegerFormulaManager
  protected val bmgr = context.getFormulaManager.getBooleanFormulaManager
  protected val prover = context.newProverEnvironment(ProverOptions.GENERATE_MODELS)

  protected val variableRegistry = mutable.Map.empty[String, InnerVarWithName]

  override def getInnerVariable(name: String) = {
    variableRegistry.getOrElseUpdate(
      name, InnerVarWithName(name=name, v=imgr.makeVariable(name))
    )
  }

  override def setObjective(minimize: Expression[Label, Int]): Unit = {
    throw new Exception("You cannot set objective with SMT")
  }

  def addInnerConstraint(constraint: InnerConstraint) = {
    prover.addConstraint(constraint)
  }

  override def addInnerConstraint(p: Predicate[InnerVarName, Int]) = {
    addInnerConstraint(convPredicate(p))
  }

  def convPredicate[L](p: Predicate[L, Int]): BooleanFormula = {
    p match {
      case GTEQ(left, right) => imgr.greaterOrEquals(convExpr(left), convExpr(right))
      case LTEQ(left, right) => imgr.lessOrEquals(convExpr(left), convExpr(right))
      case EQ(left, right) => imgr.equal(convExpr(left), convExpr(right))
      case And(ps) => bmgr.and(ps.map(p=>convPredicate(p)).asJavaCollection)
      case Or(ps) => bmgr.or(ps.map(p=>convPredicate(p)).asJavaCollection)
    }
  }

  def convExpr[L](e: Expression[L, Int]): NumeralFormula.IntegerFormula = {
    e match {
      case Add(left, right) => imgr.add(convExpr(left), convExpr(right))
      case Sub(left, right) => imgr.subtract(convExpr(left), convExpr(right))
      case Times(constant, t) => imgr.multiply(convExpr(constant), convExpr(t))
      case Constant(v) => imgr.makeNumber(v)
      case Var(v) => getInnerVariable(v.toString).v
    }
  }

  override def removeConstraint(constraintID: ConstraintID): Unit = {
    throw new Exception("You cannot remove any constraint with SMT")
  }

  override def close(): Unit = {
    context.close()
    prover.close()
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
