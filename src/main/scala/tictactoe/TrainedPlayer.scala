package tictactoe

import scala.util.Random

object TrainedPlayer {
  def getNextBestMove(state: TicTacToeState, qMatrix: Map[TicTacToeState, Map[Int, Double]], rnd: Random): Int = {
    if (qMatrix(state).nonEmpty) {
      qMatrix(state).maxBy(_._2)._1
    } else {
      val nextPossibleMoves = state.getPossibleMoves
      nextPossibleMoves(rnd.nextInt(nextPossibleMoves.size))
    }
  }

  def nextMoveForTraining(state: TicTacToeState, qMatrix: Map[TicTacToeState, Map[Int, Double]], rnd: Random): Int = {
    def pickFromCumulativeChances(valuesWithChange: Seq[(Int, Double)], roll: Double): Int = valuesWithChange match {
      case (v, c) +: _ if roll < c => v
      case (_, c) +: vs => pickFromCumulativeChances(vs, roll - c)
      case _ => throw new IllegalArgumentException
    }

    val tau: Double = 0.05

    val nextPossibleMoves = state.getPossibleMoves
    val qValuesPerMove = nextPossibleMoves.map(move => move -> qMatrix(state)(move))
    val boltzmanValuesPerMove = qValuesPerMove.map { case (move, qValue) =>
      move -> Math.exp(qValue / tau)
    }

    val result = pickFromCumulativeChances(boltzmanValuesPerMove, rnd.nextDouble() * boltzmanValuesPerMove.map(_._2).sum)
    result
  }
}

class TrainedPlayer(qMatrix: Map[TicTacToeState, Map[Int, Double]], seed: Int) extends Player {
  val rnd = new Random(seed)

  override def getNextMove(state: TicTacToeState): Int = TrainedPlayer.getNextBestMove(state, qMatrix, rnd)

}
