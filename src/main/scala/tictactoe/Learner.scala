package tictactoe

import scala.util.Random

/**
  * Created by Jordi on 30-9-2017.
  */
class Learner(learningRate: Double = 0.2d, discountFactor: Double = 0.5d, episodes: Int = 100000, seed: Int = 0) {

  val defaultQ: Double = 0
  val winReward: Double = 1
  val loseReward: Double = -1
  val neutralReward: Double = 0
  val rnd = new Random(seed)

  // Q-matrix, for performance reasons implemented as map of (state -> (action -> q)) instead of flat matrix of (state, action) -> Q
  var matrix: QMatrix = Map.empty.withDefaultValue(Map.empty.withDefaultValue(defaultQ))

  // playing as p0
  def getReward(outcome: Int): Double =
    if (outcome == p0) winReward
    else if (outcome == pDraw) neutralReward
    else loseReward

  def qLearning: QMatrix = {
    for (_ <- 0 until episodes) {
      runEpisode(rnd.nextInt(2))
    }
    matrix
  }

  def runEpisode(seed: Int): Unit = {
    def updateMatrix(reward: Double, maxQNextState: Double, previousStatesAndActions: Seq[(PureState, Int)]): Unit = previousStatesAndActions match {
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

    def statesAndActionsPerPlayer(statesAndActions: Seq[(TicTacToeState, Int)],
                                  acc: (Seq[(PureState, Int)], Seq[(PureState, Int)]) = (Seq.empty, Seq.empty)): (Seq[(PureState, Int)], Seq[(PureState, Int)]) = {
      statesAndActions match {
        case (state, action) +: rest =>
          if (state.currentPlayer == p0) {
            statesAndActionsPerPlayer(rest, ((state.pureState, action) +: acc._1, acc._2))
          } else {
            statesAndActionsPerPlayer(rest, (acc._1, (state.pureState, action) +: acc._2))
          }
        case Nil =>
          (acc._1.reverse, acc._2.reverse)
      }
    }

    def playR(mr: MoveResult, previousStatesAndActions: Seq[(TicTacToeState, Int)] = Seq.empty): Unit = mr match {
      case Left(winner) =>
        // Call updateMatrix twice, once for each player
        val (p0StatesAndActions, p1StatesAndActions) = statesAndActionsPerPlayer(previousStatesAndActions)
        // Separate previousStatesAndActions per player
        updateMatrix(getReward(winner), 0, p0StatesAndActions)
        updateMatrix(-getReward(winner), 0, p1StatesAndActions)
      case Right(state) =>
        val nextAction = TrainedPlayer.nextMoveForTraining(state, matrix, rnd)
        val nextState = state.move(nextAction)
        playR(nextState, (state, nextAction) +: previousStatesAndActions)
    }

    playR(Right(TicTacToeState.newState(seed)))
  }
}
