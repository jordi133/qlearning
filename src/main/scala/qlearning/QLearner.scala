package qlearning

import scala.util.Random


class QLearner[S, A, G <: GameState[G, S, A]](createGameFromStartingPlayer: Int => G, learningRate: Double = 0.2d, discountFactor: Double = 0.5d, episodes: Int = 100000, seed: Int = 0) {

  val defaultQ: Double = 0
  val winReward: Double = 1
  val loseReward: Double = -1
  val neutralReward: Double = 0
  val rnd = new Random(seed)

  // Q-matrix, for performance reasons implemented as map of (state -> (action -> q)) instead of flat matrix of (state, action) -> Q
  var qMatrix: QMatrix[S, A] = Map.empty.withDefaultValue(Map.empty.withDefaultValue(defaultQ))

  /**
    * Calculated from the perspective of p0
    *
    * @param outcome One of (pDraw, p0, p1)
    * @return winReward for p0, loseReward for p1, neutralReward for pDraw
    */
  def getReward(outcome: Int): Double =
    if (outcome == p0) winReward
    else if (outcome == pDraw) neutralReward
    else loseReward

  /**
    * Main training algorithm
    */
  def qLearning(): QMatrix[S, A] = {
    for (_ <- 0 until episodes) {
      runEpisode()
    }
    qMatrix
  }

  def runEpisode(): Unit = {
    val startingPlayer = rnd.nextInt(2)
    playGame(Right(createGameFromStartingPlayer(startingPlayer)))
  }

  def playGame(mr: MoveResult[G], previousStatesAndActions: Seq[(G, A)] = Seq.empty): Unit = mr match {
    case Left((winner, _)) =>
      // Separate previousStatesAndActions per player
      val (p0StatesAndActions, p1StatesAndActions) = separateStatesAndActionsPerPlayer(previousStatesAndActions, (Seq.empty, Seq.empty))
      // Call updateMatrix twice, once for each player
      updateMatrix(getReward(winner), 0, p0StatesAndActions)
      updateMatrix(-getReward(winner), 0, p1StatesAndActions)
    case Right(state) =>
      val nextAction = TrainedPlayer.nextMoveForTraining[S, A, G](state, qMatrix, rnd)
      val nextState = state.move(nextAction)
      playGame(nextState, (state, nextAction) +: previousStatesAndActions)
  }

  /**
    * Combines Seq.partition with a map from ConnectFourState to ConnectFourState.pureState
    */
  def separateStatesAndActionsPerPlayer(statesAndActions: Seq[(GameState[_, S, A], A)],
                                        acc: (Seq[(S, A)], Seq[(S, A)])): (Seq[(S, A)], Seq[(S, A)]) = {
    statesAndActions match {
      case (state, action) +: rest =>
        if (state.currentPlayer == p0) {
          separateStatesAndActionsPerPlayer(rest, ((state.pureState, action) +: acc._1, acc._2))
        } else {
          separateStatesAndActionsPerPlayer(rest, (acc._1, (state.pureState, action) +: acc._2))
        }
      case Nil =>
        (acc._1.reverse, acc._2.reverse)
    }
  }

  /**
    * Updates the qMatrix based on the played game
    */
  def updateMatrix(reward: Double, maxQNextState: Double, previousStatesAndActions: Seq[(S, A)]): Unit = previousStatesAndActions match {
    case Nil =>
    case (state, action) +: rest =>
      val currentQ = qMatrix(state)(action)
      // from https://en.wikipedia.org/wiki/Q-learning
      val newQ: Double = currentQ + learningRate * (reward + discountFactor * maxQNextState - currentQ)
      val updatedQ = qMatrix(state).updated(action, newQ)
      qMatrix = qMatrix.updated(state, updatedQ)
      val maxQCurrentState = qMatrix(state).values.max

      updateMatrix(0, maxQCurrentState, rest)
  }
}
