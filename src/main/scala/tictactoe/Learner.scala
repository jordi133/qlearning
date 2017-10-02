package tictactoe

import scala.util.Random

/**
  * Created by Jordi on 30-9-2017.
  */
object Learner {

  val learningRate = 0.1d
  val discountFactor = 0.5d


  val episodes = 250000
  val seed = 0
  val rnd = new Random(seed)

  // playing as p0
  def getReward(outcome: Int): Double =
    if (outcome == p0) 1
    else if (outcome == pDraw) 0
    else -1

  def qLearning: Map[(TicTacToeState, Player), Double] = {
    var matrix: Map[(TicTacToeState, Int), Double] = Map.empty.withDefaultValue(-1)

    def maxQForState(state: TicTacToeState): Double = {
      val filteredMatrix = matrix.filter(_._1._1 == state)
      if (filteredMatrix.nonEmpty) filteredMatrix.values.max
      else 0
    }

    def updateMatrix(reward: Double, previousStatesAndActions: Seq[(TicTacToeState, Int)]): Unit = previousStatesAndActions match {
      case Nil =>
      case (state, action) +: rest =>
        val currentQ = matrix((state, action))
        val newQ: Double = currentQ + learningRate * (reward - currentQ) // from https://en.wikipedia.org/wiki/Q-learning, omits maxQ of next state as we're in final state
        matrix = matrix.updated((state, action), newQ)

        updateMatrix(reward * discountFactor, rest)
    }

    def playR(mr: MoveResult, previousStatesAndActions: Seq[(TicTacToeState, Int)] = Seq.empty): Unit = mr match {
      case Left(winner) =>
        updateMatrix(getReward(winner), previousStatesAndActions)
      case Right(state) =>
        val possibleMoves = state.getPossibleMoves
        val nextAction = possibleMoves(rnd.nextInt(possibleMoves.size))
        val nextState = state.move(nextAction)
        playR(nextState, (state, nextAction) +: previousStatesAndActions)
    }

    for (_ <- 0 until episodes) {
      val state = TicTacToeState.newState(rnd.nextInt(2))
      playR(Right(state))
    }

    matrix
  }
}
