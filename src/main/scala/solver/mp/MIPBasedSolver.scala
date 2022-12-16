package xyz.mojashi
package solver.mp

import solver.ParikhAutomatonSolver

import com.google.ortools.linearsolver.{MPConstraint, MPSolver, MPVariable}

abstract class MIPBasedSolver[In, State, Label, Value: Numeric]
(
  val pa: ParikhAutomaton[In, State, Label, Value]
) extends ParikhAutomatonSolver[In, State, Label, Value] {
  type EdgeID = Edge[State]#EdgeID

  val labels = pa.voa.transitions.flatMap(t => t.out.keys).toSet

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

  def isConstantObjective(): Boolean = {
    objective match {
      case Constant(_) => true
      case _ => false
    }
  }

  def getMPVariable(name: String, init: MPVariable=>Unit = _=>{}): MPVariable = {
    val ret = mpSolver.lookupVariableOrNull(name)
    if(ret == null) {
      val newV = mpSolver.makeVar(-MPSolver.infinity(), MPSolver.infinity(), false, name)
      init(newV)
      newV
    } else {
      ret
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


  def getMPVariableForLabel(label: Label): MPVariable =
    getMPVariable(s"LABEL_VAR{$label}")

  val IntegerNumEdgeUsed = false
  def getMPVariableForNumEdgeUsed(edgeID: EdgeID) = {
    getMPVariable(s"NUM_EDGE_USED{$edgeID}", v=>{
      v.setInteger(IntegerNumEdgeUsed)
      v.setLb(0)
    })
  }

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

  initBaseConstraint

  def initBaseConstraint = {
    initCalcParikhImageConstraint
    initEulerConstraint
  }

  def initCalcParikhImageConstraint = {
    labels.toSeq.map(label => {
      val cons = getConstraint(s"CALC_PARIKH_CONSTRAINT{$label}")

      cons.setBounds(0, 0)
      cons.setCoefficient(getMPVariableForLabel(label), -1)

      pa.voa.transitions.filter(t =>
        m.gt(t.out.getOrElse(label, m.zero), m.zero)
      ).foreach(t => {
        cons.setCoefficient(
          getMPVariableForNumEdgeUsed(t.id),
          m.toDouble(t.out.getOrElse(label, m.zero))
        )
      })

      cons
    })
  }

  def initEulerConstraint = {
    pa.voa.states.map(s => {
      val cons = getConstraint(s"EULER_CONSTRAINT{$s}")

      s match {
        case pa.voa.start => cons.setBounds(-1, -1)
        case pa.voa.fin => cons.setBounds(1, 1)
        case _ => cons.setBounds(0, 0)
      }

      pa.voa.sourceFrom(s).foreach(t =>
        cons.setCoefficient(getMPVariableForNumEdgeUsed(t.id), -1)
      )
      pa.voa.targetTo(s).foreach(t => {
        val v = getMPVariableForNumEdgeUsed(t.id)
        cons.setCoefficient(v, cons.getCoefficient(v) + 1)
      })

      cons
    })
  }
}
