package qlearning

import scala.util.Random

object TrainedPlayer {
  /**
    * Selects next best move, or a random move if this state has not been trained yet
    */
  def getNextBestMove[S, A, G <: GameState[G, S, A]](state: G, qMatrix: QMatrix[S, A], rnd: Random): A = {
    if (qMatrix.contains(state.pureState) && qMatrix(state.pureState).nonEmpty) {
      qMatrix(state.pureState).maxBy(_._2)._1
    } else {
      val nextPossibleMoves = state.getPossibleMoves
      nextPossibleMoves(rnd.nextInt(nextPossibleMoves.size))
    }
  }

  /**
    * Picks a next move randomly based on boltzman distribution (highest Q value has highest chance_
    */
  def nextMoveForTraining[S, A, G <: GameState[G, S, A]](state: G, qMatrix: QMatrix[S, A], rnd: Random): A = {
    def pickFromCumulativeChances(valuesWithChange: Seq[(A, Double)], roll: Double): A = valuesWithChange match {
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

class TrainedPlayer[S,A, G <: GameState[G, S, A]](matrix: QMatrix[S, A], seed: Int) extends Player[A, G] {

  val rnd = new Random(seed)

  override def getNextMove(state: G): A = TrainedPlayer.getNextBestMove(state, matrix, rnd)

}
