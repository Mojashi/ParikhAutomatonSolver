package xyz.mojashi

trait TransducerTransition[In, Out, State] extends Transition[In, State] {
  def out: Out
}
