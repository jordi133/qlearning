package qlearning

trait Player[A, G <: GameState[_, _, A]] {
  def getNextMove(state: G): A
}
