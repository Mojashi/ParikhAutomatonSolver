package xyz.mojashi
package solver.algorithm

import solver.BaseSolver

import xyz.mojashi.formula.{Add, Constant, EQ, Expression, Times, Var}

trait CalcParikhConstrainedSolver[In, State, Label, Value, InnerValue] extends BaseSolver[In, State, Label, Value, InnerValue] {

  def initCalcParikhImageConstraint(implicit m: Numeric[InnerValue],  m2: Numeric[Value], c: NumericCast[Value, InnerValue]): Unit = {
    labels.toSeq.foreach(label => {
      addInnerAtomConstraint(
        EQ(
          Var[InnerVarName, InnerValue](getInnerVariableForLabel(label).name),
          pa.voa.transitions.filter(t =>
            m.gt(c.cast(t.out.getOrElse[Value](label, m2.zero)), m.zero)
          ).map(t =>
            Times(
              Constant[InnerVarName, InnerValue](c.cast(t.out.getOrElse[Value](label, m2.zero))),
              Var[InnerVarName, InnerValue](getInnerVariableForNumEdgeUsed(t.id).name)
            )
          ).fold(Constant[InnerVarName, InnerValue](m.zero))((sum, t)=>Add(sum, t))
        )
      )
    })
  }
}


trait NumericCast[From, To] {
  def cast(v: From): To
}

object Implicits {
  implicit object IntIntNumericCast extends NumericCast[Int, Int] {
    override def cast(v: Int): Int = v
  }

  implicit object IntDoubleNumericCast extends NumericCast[Int, Double] {
    override def cast(v: Int): Double = v.toDouble
  }

  implicit object DoubleDoubleNumericCast extends NumericCast[Double, Double] {
    override def cast(v: Double): Double = v
  }
}