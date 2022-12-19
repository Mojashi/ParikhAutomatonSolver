package com.github.Mojashi
package formula

sealed trait Predicate[Variable, Value] {
  def eval(assigns: Map[Variable, Value]): Boolean
}

case class Or[Variable, Value](ps: Seq[Predicate[Variable, Value]]) extends Predicate[Variable,Value] {
  override def eval(assigns: Map[Variable, Value]): Boolean = ps.exists(t => t.eval(assigns))
}

case class And[Variable, Value](ps: Seq[Predicate[Variable, Value]]) extends Predicate[Variable,Value] {
  override def eval(assigns: Map[Variable, Value]): Boolean = ps.forall(t => t.eval(assigns))
}


sealed trait AtomPredicate[Variable, Value] extends Predicate[Variable, Value] {
  def eval(assigns: Map[Variable, Value]): Boolean
}

case class EQ[Variable, Value: Numeric]
(
  left: Expression[Variable, Value],
  right: Expression[Variable, Value],
) extends AtomPredicate[Variable, Value] {
  def eval(assigns: Map[Variable, Value]): Boolean =
    implicitly[Numeric[Value]].equiv(left.eval(assigns), right.eval(assigns))
}

case class GTEQ[Variable, Value: Numeric]
(
  left: Expression[Variable, Value],
  right: Expression[Variable, Value],
) extends AtomPredicate[Variable, Value] {
  def eval(assigns: Map[Variable, Value]): Boolean =
    implicitly[Numeric[Value]].gteq(left.eval(assigns), right.eval(assigns))
}


case class LTEQ[Variable, Value: Numeric]
(
  left: Expression[Variable, Value],
  right: Expression[Variable, Value],
) extends AtomPredicate[Variable, Value] {
  def eval(assigns: Map[Variable, Value]): Boolean =
    implicitly[Numeric[Value]].lteq(left.eval(assigns), right.eval(assigns))
}



