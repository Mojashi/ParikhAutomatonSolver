package xyz.mojashi

trait Graph[State] {
  def transitions: Seq[Edge[State]]
  val states: Seq[State]

  def findTransitionByID(id: Edge[State]#EdgeID): Option[Edge[State]]
  def sourceFrom(s: State): Seq[Edge[State]]
  def targetTo(s: State): Seq[Edge[State]]
}
