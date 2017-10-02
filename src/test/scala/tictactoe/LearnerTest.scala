package tictactoe

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by Jordi on 1-10-2017.
  */
class LearnerTest extends WordSpec with Matchers {
  "qLearning" should {
    "print sensible results" in {
      val result = Learner.qLearning

      val nearlyFinishedMatrix = result.filter { case ((state, _), q) =>
        state.getPossibleMoves.size == 1 && (state.state & 1 << 18) == 0
      }

      nearlyFinishedMatrix.foreach { case ((state, action), q) =>
        println(state)
        println(s"$action -> q = $q")
      }

      val justStartingMatrix = result.filter { case ((state, _), q) =>
        state.getPossibleMoves.size == 9 && (state.state & 1 << 18) == 0
      }

      justStartingMatrix
        .map { case ((state, action), q) => (action, q) }
        .toSeq.sortBy(_._2)
        .foreach { case (action, q) =>
          println(s"$action: q = $q")
        }

      // play game with learned matrix
      var game: MoveResult = Right(TicTacToeState.newState(p0))
      import TicTacToeState._

      while (game.isRight) {
        val state = game.right.get
        if (!result.exists { case ((st, _), _) => st == state }) {
          println(s"Empty matrix for next move in:\n$state")
        }
        val nextMove = result.filter { case ((st, _), _) => st == state }.maxBy(_._2)._1._2
        game = game.move(nextMove)
        println(game.asString)

        // opponent move
        if (game.isRight) {
          game = game.move(game.right.get.getPossibleMoves.head)
        }

      }

    }


  }
}
