package xyz.mojashi

case class SolverConfig
(
  linearRelaxed: Boolean,
  ensureMinimum: Boolean,
  ensureConnected: Boolean,
)
object Configs {
  val ExactConfig = SolverConfig(linearRelaxed=false,ensureMinimum=true, ensureConnected=true)
}

trait ParikhAutomatonSolver[In, State, Label, Value] {
  def solve
  (
    pa: ParikhAutomaton[In, State, Label, Value],
    config: SolverConfig = Configs.ExactConfig,
    minimize: Expression[Label] = Constant(0),
  ): Option[Map[Edge[State]#EdgeID, Double]]
}