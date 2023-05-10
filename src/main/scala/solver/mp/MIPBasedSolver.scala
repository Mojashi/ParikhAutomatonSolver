package com.github.Mojashi
package solver.mp

import solver.{BaseSolver, ParikhAutomatonSolver}

import com.google.ortools.linearsolver.{MPConstraint, MPSolver, MPVariable}
import com.typesafe.scalalogging.Logger
import com.github.Mojashi.automaton.ParikhAutomaton
import com.github.Mojashi.formula.{And, AtomPredicate, Constant, EQ, Expression, GTEQ, LTEQ, Or, Predicate, Sub}
import com.github.Mojashi.graph.{Edge, EdgeID}
import com.github.Mojashi.solver.common.Implicits.IntIntNumericCast
import com.github.Mojashi.solver.common.{CalcParikhConstrainedSolver, EulerConstrainedSolver, NumericCast}

import scala.collection.mutable

case class ExplicitMPConstraint
(
  v: Map[String, Double],
  range: (Double, Double)
)

abstract class MIPBasedSolver[In, Label, Value: Numeric]
(
  val pa: ParikhAutomaton[In, Label, Value],
  val lpRelaxed:Boolean = false,
  val ensureConnectivity: Boolean = true,
  val underlyingSolver: ORToolsMIPSolver = ORToolsMIPSolver.SCIP,
  val singleTimeout: Int = 30000
) (implicit cast: NumericCast[Value, Double])
  extends BaseSolver[In, Label, Value, Double]
    with ParikhAutomatonSolver[In, Label, Value]
    with EulerConstrainedSolver[In, Label, Value, Double]
    with CalcParikhConstrainedSolver[In, Label, Value, Double] {

  override type InnerExpr = Map[String, Double]
  override type InnerVar = MPVariable
  override type InnerConstraint = ExplicitMPConstraint


  if(!lpRelaxed && !underlyingSolver.supportInt) {
    throw new RuntimeException(s"${underlyingSolver.name} doesn't support integer variables. Try another solver or set lpRelaxed = true.")
  }
  val mpSolver = MPSolver.createSolver(underlyingSolver.name)
  mpSolver.setTimeLimit(singleTimeout)


  val m = implicitly[Numeric[Value]]

  val objective = Constant(0)

  override def setObjective(minimize: Expression[Either[Label, EdgeID], Value]): Unit = {
    val (coeffs, constant) = getCoefficients(minimize)
    val obj = mpSolver.objective()
    obj.clear()

    for ((t, coeff) <- coeffs) {
      t match {
        case Left(label) =>
          obj.setCoefficient(getInnerVariableForLabel(label).v, m.toDouble(coeff))
        case Right(edgeId) =>
          obj.setCoefficient(getInnerVariableForNumEdgeUsed(edgeId).v, m.toDouble(coeff))
      }
    }
    obj.setMinimization()
  }

  def isConstantObjective(): Boolean = {
    objective match {
      case Constant(_) => true
      case _ => false
    }
  }

  val innerVars = mutable.Set[InnerVarWithName]()

  override def getInnerVariable(name: InnerVarName): InnerVarWithName = {
    val ret = mpSolver.lookupVariableOrNull(name)
    if(ret == null) {
      val varWithName = InnerVarWithName(
        name = name,
        v = mpSolver.makeVar(-MPSolver.infinity(), MPSolver.infinity(), false, name)
      )
      innerVars.add(varWithName)
      varWithName

    } else {
      InnerVarWithName(
        name=name,
        v=ret,
      )
    }
  }

  def getConstraint(name: ConstraintID): MPConstraint = {
    val ret = mpSolver.lookupConstraintOrNull(name)
    if (ret == null) {
      mpSolver.makeConstraint(name)
    } else {
      ret
    }
  }

  def convAtomPredicate(p: AtomPredicate[InnerVarName, Double]): ExplicitMPConstraint = {
    val (range, left, right): ((Double,Double), Expression[InnerVarName, Double],Expression[InnerVarName, Double]) = p match {
      case EQ(left, right) => ((0, 0), left, right)
      case LTEQ(left, right) => ((-MPSolver.infinity(), 0), left, right)
      case GTEQ(left, right) => ((0, MPSolver.infinity()), left, right)
    }

    val (coeffs, c) = getCoefficients(Sub(left, right))

    val newRange = (range._1 - c, range._2 - c)

    ExplicitMPConstraint(
      v = coeffs.map { case (name, v) => (name, v) },
      range = newRange,
    )
  }

  override def addConstraint(constraint: Predicate[Label, Value], constraintID: ConstraintID = ""): ConstraintID = {
    if(constraintID != "") {
      Logger("MIPBasedSolver").warn(s"ConstraintID was specified as $constraintID but is ignored")
    }
    addInnerConstraint(convParikhPredicateToInner(constraint))
    ""
  }

  def addInnerConstraint(cons: InnerConstraint): ConstraintID = {
    val act = mpSolver.makeConstraint()
    cons.v.foreach{case (key, v) =>
      act.setCoefficient(getInnerVariable(key).v, v)
    }
    act.setBounds(cons.range._1, cons.range._2)
    act.name()
  }

  override def addAtomPredicateConstraint (constraint: AtomPredicate[Label, Value], constraintID: ConstraintID): ConstraintID = {
    addInnerAtomConstraint(convParikhAtomPredicateToInner(constraint))
  }

  override def addInnerAtomConstraint(p: AtomPredicate[InnerVarName, Double]): ConstraintID = {
    addInnerConstraint(convAtomPredicate(p))
  }

  override def removeConstraint(constraintID: ConstraintID): Unit = {
    val cons = mpSolver.lookupConstraintOrNull(constraintID)
    if(cons == null) return

    cons.setBounds(-MPSolver.infinity(), MPSolver.infinity())
    cons.delete()
  }

  override def addInnerConstraint(p: Predicate[InnerVarName, Double]): Unit = {
    p match {
      case Or(_) =>
        throw new NotImplementedError("MIP Solver cannot handle 'OR'")

      case And(ps) => ps.foreach(p => addInnerConstraint(p))
      case atomP: AtomPredicate[InnerVarName, Double] => addInnerAtomConstraint(atomP)
    }
  }

  def constraintNEUisInteger = {
    pa.voa.transitions.foreach(t =>
      getInnerVariableForNumEdgeUsed(t.id).v.setInteger(true)
    )
  }

  def solveInner: Option[Map[InnerVarName, Double]] = {
    val result = mpSolver.solve()

    Logger("result").debug(result.toString)
    if(result == MPSolver.ResultStatus.ABNORMAL || result == MPSolver.ResultStatus.NOT_SOLVED) {
      throw new Error("timeout or some error has occurred")
    }
    if (result != MPSolver.ResultStatus.FEASIBLE && result != MPSolver.ResultStatus.OPTIMAL) {
      return None
    }

    val model: Map[InnerVarName, Double] = innerVars.map(t =>
      (t.name, t.v.solutionValue())
    ).toMap
    Some(model)
  }

  initEulerConstraint
  initCalcParikhImageConstraint
  constraintPAConstraint
  constraintNumEdgeUsedIsPositive
  if(!lpRelaxed) constraintNEUisInteger
}
