package tictactoe

import scala.util.Random

object TrainedPlayer {
  /**
    * Selects next best move, or a random move if this state has not been trained yet
    */
  def getNextBestMove(state: TicTacToeState, qMatrix: QMatrix, rnd: Random): Action = {
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
  def nextMoveForTraining(state: TicTacToeState, qMatrix: QMatrix, rnd: Random): Action = {
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

    pickFromCumulativeChances(boltzmanValuesPerMove, rnd.nextDouble() * boltzmanValuesPerMove.map(_._2).sum)
  }
}

class TrainedPlayer(matrix: QMatrix, seed: Int) extends Player {
  val rnd = new Random(seed)

  override def getNextMove(state: TicTacToeState): Action = TrainedPlayer.getNextBestMove(state, matrix, rnd)
}
