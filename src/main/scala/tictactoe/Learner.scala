package tictactoe

import scala.util.Random

/**
  * Created by Jordi on 30-9-2017.
  */
object Learner {

  val learningRate = 0.1d
  val discountFactor = 0.5d


  val episodes = 2500
  val seed = 0
  val rnd = new Random(seed)

  // playing as p0
  def getReward(outcome: Int): Double =
    if (outcome == p0) 1
    else if (outcome == pDraw) 0
    else -1

  def qLearning: Map[(TicTacToeState, Player), Double] = {
    var matrix: Map[(TicTacToeState, Int), Double] = Map.empty.withDefaultValue(-1)

    //    var stateActionToNextStateMap: Map[(TicTacToeState, Int), Set[TicTacToeState]] = Map.empty.withDefaultValue(Set.empty)

    def maxQForState(state: TicTacToeState): Double = {
      val filteredMatrix = matrix.filter(_._1._1 == state)
      if (filteredMatrix.nonEmpty) filteredMatrix.values.max
      else 0
    }

    def updateMatrix(reward: Double, previousStatesAndActions: Seq[(TicTacToeState, Int)]): Unit = previousStatesAndActions match {
      case Nil =>
      case (state, action) +: rest =>
        val currentQ = matrix((state, action))

//        val possibleNextStates = state.getPossibleMoves.map(state.forceMove)

//        val maxQNextState = possibleNextStates.map(nextState => maxQForState(nextState)).max

//        val newQ: Double = currentQ + learningRate * (discountFactor * maxQNextState - currentQ) // from https://en.wikipedia.org/wiki/Q-learning, omits reward as we're not in final state
                val newQ: Double = currentQ + learningRate * (reward - currentQ) // from https://en.wikipedia.org/wiki/Q-learning, omits maxQ of next state as we're in final state
        matrix = matrix.updated((state, action), newQ)

        updateMatrix(reward * discountFactor, rest)
    }

//    def updateMatrixFromEndstate(reward: Double, previousStatesAndActions: Seq[(TicTacToeState, Int)]): Unit = previousStatesAndActions match {
//      case Nil =>
//        throw new IllegalArgumentException("Cannot updateMatrixFromEndstate when not in endstate (previousStatesAndActions = Nil)")
//      case (state, action) +: rest =>
//        // Regardless, this state combined with this action leads to reward, so q should be equal to reward
//        matrix = matrix.updated((state, action), reward)
//        //        val currentQ = matrix((state, action))
//        //        val newQ: Double = currentQ + learningRate * (reward - currentQ) // from https://en.wikipedia.org/wiki/Q-learning, omits maxQ of next state as we're in final state
//        //        matrix = matrix.updated((state, action), newQ)
//
//        updateMatrix(rest)
//    }

    //    def updateStateActionMap(previousStatesAndActions: Seq[(TicTacToeState, Int)]): Unit = previousStatesAndActions match {
    //      case last +: secondLast +: rest =>
    //        val resultsFromSecondLast = stateActionToNextStateMap(secondLast)
    //        stateActionToNextStateMap = stateActionToNextStateMap.updated(secondLast, resultsFromSecondLast + last._1)
    //        updateStateActionMap(secondLast +: rest)
    //      case _ =>
    //    }

    def playR(mr: MoveResult, previousStatesAndActions: Seq[(TicTacToeState, Int)] = Seq.empty): Unit = mr match {
      case Left(winner) =>
        //        updateStateActionMap(previousStatesAndActions)
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

  def qAlgo: Map[(TicTacToeState, Player), Double] = {
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

        // TODO rewrite to only use backtracking when game is over
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
