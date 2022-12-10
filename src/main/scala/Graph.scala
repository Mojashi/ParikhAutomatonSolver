package xyz.mojashi

trait Graph[State] {
  def transitions: Seq[Edge[State]]
  def sourceFrom(s: State): Seq[Edge[State]]
  def targetTo(s: State): Seq[Edge[State]]
}
