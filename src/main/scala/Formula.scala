package xyz.mojashi

trait AtomPredicate[Variable, Value] {
  def eval(assigns: Map[Variable, Value]): Boolean
}

case class EQ[Variable, Value: Numeric]
(
  left: Expression[Variable, Value],
  right: Expression[Variable, Value],
) extends AtomPredicate[Variable, Value] {
  def eval(assigns: Map[Variable, Value]): Boolean =
    implicitly[Numeric[Value]].eq(left.eval(assigns), right.eval(assigns))
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



