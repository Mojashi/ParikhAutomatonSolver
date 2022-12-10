package xyz.mojashi

trait VectorOutputAutomaton[In, State, Label] extends NFA[In, Map[Label, Double], State] {
  def runWithAddition(in: Seq[In]): Set[Map[Label, Double]]
}
