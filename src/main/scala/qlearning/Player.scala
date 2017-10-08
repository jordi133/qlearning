package qlearning

trait Player[G <: GameState[_]] {
  def getNextMove(state: G): Int
}
