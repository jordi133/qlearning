package tictactoe

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by Jordi on 1-10-2017.
  */
class LearnerTest extends WordSpec with Matchers {
  "qAlgo" should {
    "print sensible results" in {
      val result = Learner.qAlgo

      val nearlyFinishedMatrix = result.filter { case ((state, _), q) =>
          state.getPossibleMoves.size == 1
      }

      nearlyFinishedMatrix.foreach { case ((state, action), q) =>
          println(state)
          println(s"$action -> q = $q")
      }

      val justStartingMatrix = result.filter { case ((state, _), q) =>
          state.getPossibleMoves.size == 9
      }

      justStartingMatrix.foreach { case ((state, action), q) =>
          println(state)
          println(s"$action -> q = $q")
      }


      // play game with learned matrix
      var game: MoveResult = Right(TicTacToeState.newState(p0))
      import TicTacToeState._

      while (game.isRight) {
        val state = game.right.get
        val nextMove = result.filter{case ((st, _), _) => st == state}.maxBy(_._2)._1._2
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
