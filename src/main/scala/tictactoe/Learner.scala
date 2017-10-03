package tictactoe

import scala.util.Random

/**
  * Created by Jordi on 30-9-2017.
  *
  * TODO: split learning in several parts: first train against random opponent and then against trained opponent?
  */
class Learner(learningRate: Double = 0.2d, discountFactor: Double = 0.5d, episodes: Int = 10000, seed: Int = 0) {

  val defaultQ: Double = 0
  val winReward: Double = 1
  val loseReward: Double = -1
  val neutralReward: Double = 0
  val rnd = new Random(seed)

  // Q-matrix, for performance reasons implemented as map of (state -> (action -> q)) instead of flat matrix of (state, action) -> Q
  var matrix: Map[TicTacToeState, Map[Int, Double]] = Map.empty.withDefaultValue(Map.empty.withDefaultValue(defaultQ))

  // playing as p0
  def getReward(outcome: Int): Double =
    if (outcome == p0) winReward
    else if (outcome == pDraw) neutralReward
    else loseReward

  def qLearning: Map[TicTacToeState, Map[Int, Double]] = {
    for (_ <- 0 until episodes) {
      runEpisode(rnd.nextInt(2))
    }
    matrix
  }

  def runEpisode(seed: Int): Unit = {
    def updateMatrix(reward: Double, maxQNextState: Double, previousStatesAndActions: Seq[(TicTacToeState, Int)]): Unit = previousStatesAndActions match {
      case Nil =>
      case (state, action) +: rest =>
        val currentQ = matrix(state)(action)
        // from https://en.wikipedia.org/wiki/Q-learning
        val newQ: Double = currentQ + learningRate * (reward + discountFactor * maxQNextState - currentQ)
        val updatedQ = matrix(state).updated(action, newQ)
        matrix = matrix.updated(state, updatedQ)
        val maxQCurrentState = matrix(state).values.max

        updateMatrix(0, maxQCurrentState, rest)
    }

    def playR(mr: MoveResult, previousStatesAndActions: Seq[(TicTacToeState, Int)] = Seq.empty): Unit = mr match {
      case Left(winner) =>
        updateMatrix(getReward(winner), 0, previousStatesAndActions)
      case Right(state) =>
        val possibleMoves = state.getPossibleMoves
                val nextAction = nextMoveForTraining(state)
//        val nextAction = possibleMoves(rnd.nextInt(possibleMoves.size))
        val nextState = state.move(nextAction)
        playR(nextState, (state, nextAction) +: previousStatesAndActions)
    }

    playR(Right(TicTacToeState.newState(seed)))
  }

  def bestNextMove(state: TicTacToeState): Int = {
    if (matrix(state).nonEmpty) {
      matrix(state).maxBy(_._2)._1
    } else {
      val nextPossibleMoves = state.getPossibleMoves
      nextPossibleMoves(rnd.nextInt(nextPossibleMoves.size))
    }
  }


  def nextMoveForTraining(state: TicTacToeState): Int = {
    def pickFromCumulativeChances(valuesWithChange: Seq[(Int, Double)], roll: Double): Int = valuesWithChange match {
      case (v, c) +: _ if roll < c => v
      case (_, c) +: vs => pickFromCumulativeChances(vs, roll - c)
      case _ => throw new IllegalArgumentException
    }

    val tau: Double = 0.05

    val nextPossibleMoves = state.getPossibleMoves
    val qValuesPerMove = nextPossibleMoves.map(move => move -> matrix(state)(move))
    val boltzmanValuesPerMove = qValuesPerMove.map { case (move, qValue) =>
      move -> Math.exp(qValue / tau)
    }

    println(s"boltzmanValuesPerMove: ${boltzmanValuesPerMove.mkString(" | ")}")

    val result = pickFromCumulativeChances(boltzmanValuesPerMove, rnd.nextDouble() * boltzmanValuesPerMove.map(_._2).sum)
    println(s"result: $result")
    result
  }

}
