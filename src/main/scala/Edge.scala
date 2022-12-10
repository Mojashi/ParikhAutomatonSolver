package xyz.mojashi


trait Edge[State] {
  type EdgeID = Int
  def from: State
  def to: State
  def id: EdgeID
}
