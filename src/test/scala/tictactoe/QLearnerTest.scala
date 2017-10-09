package tictactoe

import org.scalatest.{Matchers, WordSpec}

import qlearning._
import MoveResultImplicit.ImplicitState

/**
  * Created by Jordi on 1-10-2017.
  */
class QLearnerTest extends WordSpec with Matchers {

  "qLearning" should {
    val learner = new QLearner[Int, TicTacToeState](learningRate = 0.2d, discountFactor = 0.5d, episodes = 10000, seed = 0)
    val result = learner.qLearning(startingPlayer => TicTacToeState.newState(startingPlayer))
    "give highest Q Value to the move preventing the opponent from winning" in {
      val state = TicTacToeState.newState(p0)
        .move(0) // O - O
        .move(3) // X - -
        .move(2) // - - -
        .right.get

      val indexToPreventWin = 1

      val otherIndices = Seq(4, 5, 6, 7, 8)

      otherIndices.foreach { i =>
        result(state.pureState)(i) should be < result(state.pureState)(indexToPreventWin)
      }
    }
    "Give highest Q Value to winning move" in {
      val state = TicTacToeState.newState(p0)
        .move(0) // O - O
        .move(3) // X - -
        .move(2) // - - X
        .move(8)
        .right.get
      val winningIndex = 1
      val otherIndices = Seq(4, 5, 6, 7)

      otherIndices.foreach { i =>
        result(state.pureState)(i) should be < result(state.pureState)(winningIndex)
      }

    }
  }
}
