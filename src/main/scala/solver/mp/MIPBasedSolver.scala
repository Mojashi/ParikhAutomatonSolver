package xyz.mojashi
package solver.mp

import solver.ParikhAutomatonSolver

import com.google.ortools.linearsolver.{MPSolver, MPVariable}

abstract class MIPBasedSolver[In, State, Label, Value: Numeric]
(
  val pa: ParikhAutomaton[In, State, Label, Value]
) extends ParikhAutomatonSolver[In, State, Label, Value] {

  val mpSolver = MPSolver.createSolver("SCIP")
  val m = implicitly[Numeric[Value]]

  val objective = Constant(0)

  override def setObjective(minimize: Expression[Label, Value]): Unit = {
    val (coeffs, constant) = getCoefficients(minimize)
    val obj = mpSolver.objective()
    obj.clear()

    for ((t, coeff) <- coeffs) {
      obj.setCoefficient(getMPVariableForLabel(t), m.toDouble(coeff))
    }
    obj.setMinimization()
  }

  def getMPVariable(name: String): MPVariable = {
    val ret = mpSolver.lookupVariableOrNull(name)
    if(ret == null) {
      mpSolver.makeVar(-MPSolver.infinity(), MPSolver.infinity(), false, name)
    } else {
      ret
    }
  }

  def getMPVariableForLabel(label: Label): MPVariable =
    getMPVariable("LABEL_VAR_" + label)
  def getMPVariableForEdgeUsedCount()

  override def addConstraint(constraint: AtomPredicate[Label, Value], constraintID: ConstraintID): ConstraintID = {
    val (cons,left,right) = constraint match {
      case EQ(left, right) => (mpSolver.makeConstraint(0, 0, constraintID), left, right)
      case LTEQ(left, right) => (mpSolver.makeConstraint(-MPSolver.infinity(), 0, constraintID), left, right)
      case GTEQ(left, right) => (mpSolver.makeConstraint(0, MPSolver.infinity(), constraintID) , left, right)
    }

    val (coeffs, c) = getCoefficients(Sub(left, right))

    cons.setBounds(cons.lb() - m.toDouble(c), cons.ub() - m.toDouble(c))

    for ((t, coeff) <- coeffs) {
      cons.setCoefficient(getMPVariableForLabel(t), m.toDouble(coeff))
    }

    if(constraintID == "")
      cons.name()
    else
      constraintID
  }



  override def removeConstraint(constraintID: ConstraintID): Unit = {
    val cons = mpSolver.lookupConstraintOrNull(constraintID)
    if(cons == null) return

    cons.setBounds(-MPSolver.infinity(), MPSolver.infinity())
    cons.delete()
  }


  def initBaseConstraint = {
    def initCalcParikhImageConstraint = {
      normalized.calcParikhImageForLP
      val parikhVars = normalized.MPParikhVars()

      normalized.keys.foreach(key => {
        parikhVars.chCountVar(key).setLb(0)
        parikhVars.chCountVar(key).setUb(0)
      })

    }
    def initEulerConstraint = {
      normalized.baseFlowConnectedConstraintInLP()

      val flowVars = normalized.MPConnectivityFlowVars()
      flowVars.flowConstraint(normalized.start).setBounds(-1, -1)
    }
  }
}
