package connectfour

import scala.util.Random
import qlearning._

object ConnectFourPlayer {
  def getRandomMove(state: ConnectFourState, rnd: Random): PlayerId = {
    val possibleMoves = state.getPossibleMoves
    possibleMoves(rnd.nextInt(possibleMoves.size))
  }
}

trait ConnectFourPlayer extends Player[ConnectFourState] {
  def getNextMove(state: ConnectFourState): Action
}
