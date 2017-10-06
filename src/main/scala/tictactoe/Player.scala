package tictactoe

import scala.util.Random

object Player {
  def getRandomMove(state: TicTacToeState, rnd: Random): PlayerId = {
    val possibleMoves = state.getPossibleMoves
    possibleMoves(rnd.nextInt(possibleMoves.size))
  }
}

trait Player {
  def getNextMove(state: TicTacToeState): Int
}