package xyz.mojashi

// Out must be monoid
trait NFA[In, Out, State] extends Graph [State] {
  type T = Transition[In, Out, State]

  val start: State
  val fin: State
  val transitions: Seq[T]
  def sourceFrom(s: State): Seq[T]
  def targetTo(s: State): Seq[T]
  def run(in: Seq[In]): Set[Seq[Out]]
}
