package connectfour

import scala.util.Random

import qlearning._

object TrainedConnectFourPlayer {
  /**
    * Selects next best move, or a random move if this state has not been trained yet
    */
  def getNextBestMove(state: ConnectFourState, qMatrix: QMatrix, rnd: Random): Action = {
    if (qMatrix(state.pureState).nonEmpty) {
      qMatrix(state.pureState).maxBy(_._2)._1
    } else {
      val nextPossibleMoves = state.getPossibleMoves
      nextPossibleMoves(rnd.nextInt(nextPossibleMoves.size))
    }
  }

  /**
    * Picks a next move randomly based on boltzman distribution (highest Q value has highest chance_
    */
  def nextMoveForTraining(state: ConnectFourState, qMatrix: QMatrix, rnd: Random): Action = {
    def pickFromCumulativeChances(valuesWithChange: Seq[(Int, Double)], roll: Double): Action = valuesWithChange match {
      case (v, c) +: _ if roll < c => v
      case (_, c) +: vs => pickFromCumulativeChances(vs, roll - c)
      case _ => throw new IllegalArgumentException
    }

    val tau: Double = 0.05

    val nextPossibleMoves = state.getPossibleMoves
    val qValuesPerMove = nextPossibleMoves.map(move => move -> qMatrix(state.pureState)(move))
    val boltzmanValuesPerMove = qValuesPerMove.map { case (move, qValue) =>
      move -> Math.exp(qValue / tau)
    }

    if (boltzmanValuesPerMove.isEmpty) {
      println(s"Something is wrong:\n${state.toString}")
    }
    pickFromCumulativeChances(boltzmanValuesPerMove, rnd.nextDouble() * boltzmanValuesPerMove.map(_._2).sum)
  }
}

class TrainedConnectFourPlayer(matrix: QMatrix, seed: Int) extends ConnectFourPlayer {
  val rnd = new Random(seed)

  override def getNextMove(state: ConnectFourState): Action = TrainedConnectFourPlayer.getNextBestMove(state, matrix, rnd)
}