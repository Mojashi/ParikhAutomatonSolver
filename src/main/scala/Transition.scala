package xyz.mojashi

trait Transition[In, Out, State] extends Edge[State] {
  def from: State
  def to: State
  def in: In
  def Out: Out
}
