package connectfour

import scala.util.Random

object Player {
  def getRandomMove(state: ConnectFourState, rnd: Random): PlayerId = {
    val possibleMoves = state.getPossibleMoves
    possibleMoves(rnd.nextInt(possibleMoves.size))
  }
}

trait Player {
  def getNextMove(state: ConnectFourState): Int
}