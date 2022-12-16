package xyz.mojashi

// Out must be monoid
trait NFA[In, State] extends Graph [State] {
  type T = Transition[In, State]

  val start: State
  val fin: State
  val transitions: Seq[T]

  def findTransitionByID(id: T#EdgeID): Option[T]
  def sourceFrom(s: State): Seq[T]
  def targetTo(s: State): Seq[T]
  def accept(in: Seq[In]): Boolean
}
