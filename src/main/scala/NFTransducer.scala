package xyz.mojashi

// Out must be monoid
trait NFTransducer[In, Out, State] extends NFA[In, State] {
  override type T = TransducerTransition[In, Out, State]

  val start: State
  val fin: State
  val transitions: Seq[T]

  def findTransitionByID(id: T#EdgeID): Option[T]
  def sourceFrom(s: State): Seq[T]
  def targetTo(s: State): Seq[T]
  def run(in: Seq[In]): Set[Seq[Out]]
}
