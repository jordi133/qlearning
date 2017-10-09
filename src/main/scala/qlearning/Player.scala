package qlearning

trait Player[G <: GameState[_, _]] {
  def getNextMove(state: G): Int
}
