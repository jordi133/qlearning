package tictactoe

import scala.util.Random

/**
  * Created by Jordi on 30-9-2017.
  */
object Learner {


  def qAlgo: Map[(TicTacToeState, Player), Double] = {

    val learningRate = 0.1d
    val discountFactor = 0.5d

    // playing as p0
    def getReward(outcome: Int): Double =
      if (outcome == p0) 1
      else if (outcome == pDraw) 0
      else -1

    val episodes = 1000
    val seed = 0
    val rnd = new Random(seed)

    var matrix: Map[(TicTacToeState, Int), Double] = Map.empty.withDefaultValue(0)

    def updateMatrix(reward: Double, previousStatesAndActions: Seq[(TicTacToeState, Int)]): Unit = previousStatesAndActions match {
      case Nil =>
      case (state, action) +: rest =>
        val currentQ = matrix((state, action))
        val newQ: Double = currentQ + learningRate * (reward - currentQ) // from https://en.wikipedia.org/wiki/Q-learning, omits maxQ of next state as we're in final state
        matrix = matrix.updated((state, action), newQ)
    }

    def playR(mr: MoveResult, previousStatesAndActions: Seq[(TicTacToeState, Int)] = Seq.empty): Unit = mr match {
      case Left(winner) =>
        updateMatrix(getReward(winner), previousStatesAndActions)
      case Right(state) =>
        val possibleMoves = state.getPossibleMoves

        val nextAction = possibleMoves(rnd.nextInt(possibleMoves.size))
        val nextState = state.move(nextAction)

        val maxQ = nextState match {
          case Left(_) =>
            0
          case Right(st) =>
            val qValuesForNextState = matrix.filter { case ((stKey, _), _) => stKey == st }
            if (qValuesForNextState.isEmpty) {
              0
            } else {
              qValuesForNextState.maxBy(_._2)._2
            }
        }

        val reward = nextState match {
          case Left(winner) => getReward(winner)
          case _ => 0
        }
        // update matrix
        val currentQ = matrix((state, nextAction))
        val newQ: Double = currentQ + learningRate * (reward + discountFactor * maxQ - currentQ) // from https://en.wikipedia.org/wiki/Q-learning, omits maxQ of next state as we're in final state
        matrix = matrix.updated((state, nextAction), newQ)


        playR(nextState, (state, nextAction) +: previousStatesAndActions)
    }

    for (_ <- 0 until episodes) {
      val state = TicTacToeState.newState(rnd.nextInt(2))
      playR(Right(state))
    }

    matrix
  }

}
