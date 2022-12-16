package xyz.mojashi

trait VectorOutputAutomaton[In, State, Label, Value] extends NFTransducer[In, Map[Label, Value], State] {
  def runWithAddition(in: Seq[In]): Set[Map[Label, Value]]
}
