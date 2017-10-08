package connectfour

import scala.util.Random

class QLearner(learningRate: Double = 0.2d, discountFactor: Double = 0.5d, episodes: Int = 100000, seed: Int = 0) {

  val defaultQ: Double = 0
  val winReward: Double = 1
  val loseReward: Double = -1
  val neutralReward: Double = 0
  val rnd = new Random(seed)

  // Q-matrix, for performance reasons implemented as map of (state -> (action -> q)) instead of flat matrix of (state, action) -> Q
  var qMatrix: QMatrix = Map.empty.withDefaultValue(Map.empty.withDefaultValue(defaultQ))

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
  def qLearning: QMatrix = {
    for (_ <- 0 until episodes) {
      runEpisode(rnd.nextInt(2))
    }
    qMatrix
  }

  /**
    * Improve qlearning approach by first branching out on the different gamestates:
    * - for every possible move, start a new branch by doing that move
    * - when a certain threshold is reached, split all states in groups that cannot converge to the same state
    * - (continue branching over all possible states until a next threshold is reached, from then on only pick random next moves)
    * - parallelise calculations over these groups, for every group calculate the delta they would have on the QMatrix
    * - Update QMatrix while backtracking until the moment of this parallelisation
    *   - here the max q values of the next states can not influence eachother due to states not able to converge
    * - update qmatrix for all states calculated in parallel (probably squentially to avoid race conditions)
    * - sequentiall update qmatrix for all states that came before parallelisation (now max q values of next state can influence eachother)
    */
  def improvedQLearning: QMatrix = {

    /**
      * TODO: Define/design a strategy such that after x moves, there are multiple groups of states that cannot converge to the same state
      * An example could be to first play only two moves and let the second move be opposite to the first
      * In general, when playing n moves, branch the states such that after n moves, each state has the same n positions covered
      * Challenge is to define such a strategy so that the algorithm will pass through all states evenly
      *
      * Idea:
      * Play several random moves in one game
      * After n moves, play from initial game in all possible ways to also fill up the same n positions
      * Then groups can be made on the different states and from there on calculatens can be done in parallel
      */
    val bruteForceStates = ???

    val statesGrouped = ConnectFourState.groupByNonConvergence(bruteForceStates)

    println(s"statesGrouped size = ${statesGrouped.size}")
    for (group <- statesGrouped) {
      println("Group:")
      for (s <- group) println(s.asString)
    }

    ???
  }

  def runEpisode(startingPlayer: Int): Unit = playGame(Right(ConnectFourState.newState(startingPlayer)))

  def playGame(mr: MoveResult, previousStatesAndActions: Seq[(ConnectFourState, Int)] = Seq.empty): Unit = mr match {
    case Left((winner, _)) =>
      // Separate previousStatesAndActions per player
      val (p0StatesAndActions, p1StatesAndActions) = separateStatesAndActionsPerPlayer(previousStatesAndActions, (Seq.empty, Seq.empty))
      // Call updateMatrix twice, once for each player
      updateMatrix(getReward(winner), 0, p0StatesAndActions)
      updateMatrix(-getReward(winner), 0, p1StatesAndActions)
    case Right(state) =>
      val nextAction = TrainedPlayer.nextMoveForTraining(state, qMatrix, rnd)
      val nextState = state.move(nextAction)
      playGame(nextState, (state, nextAction) +: previousStatesAndActions)
  }

  /**
    * Combines Seq.partition with a map from ConnectFourState to ConnectFourState.pureState
    */
  def separateStatesAndActionsPerPlayer(statesAndActions: Seq[(ConnectFourState, Int)],
                                        acc: (Seq[(PureState, Int)], Seq[(PureState, Int)])): (Seq[(PureState, Int)], Seq[(PureState, Int)]) = {
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
  def updateMatrix(reward: Double, maxQNextState: Double, previousStatesAndActions: Seq[(PureState, Int)]): Unit = previousStatesAndActions match {
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