package xyz.mojashi

trait Transition[In, State] extends Edge[State] {
  def in: In
}
