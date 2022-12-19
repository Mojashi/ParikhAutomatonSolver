package com.github.Mojashi
package formula

sealed trait Expression[Variable, Value] {
  def eval(assigns: Map[Variable, Value]): Value
}

case class Constant[Variable, Value]
(
  v: Value
) extends Expression[Variable, Value] {
  def eval(assigns: Map[Variable, Value]) = v
}

case class Add[Variable, Value: Numeric]
(
  left: Expression[Variable, Value],
  right: Expression[Variable, Value],
) extends Expression[Variable, Value] {
  val n = implicitly[Numeric[Value]]

  def eval(assigns: Map[Variable, Value]) =
    n.plus(left.eval(assigns: Map[Variable, Value]), right.eval(assigns: Map[Variable, Value]))
}

case class Sub[Variable, Value: Numeric]
(
  left: Expression[Variable, Value],
  right: Expression[Variable, Value],
) extends Expression[Variable, Value] {
  val n = implicitly[Numeric[Value]]

  def eval(assigns: Map[Variable, Value]) =
    n.minus(left.eval(assigns: Map[Variable, Value]), right.eval(assigns: Map[Variable, Value]))
}

case class Times[Variable, Value: Numeric]
(
  constant: Constant[Variable, Value],
  e: Expression[Variable, Value],
) extends Expression[Variable, Value] {
  val n = implicitly[Numeric[Value]]

  def eval(assigns: Map[Variable, Value]) =
    n.times(constant.eval(assigns), e.eval(assigns))
}

case class Var[Variable, Value]
(
  v: Variable
) extends Expression[Variable, Value] {
  def eval(assigns: Map[Variable, Value]) =
    assigns.get(v).get
}
