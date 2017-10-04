package tictactoe

import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

/**
  * Created by Jordi on 1-10-2017.
  */
class LearnerTest extends WordSpec with Matchers {

  "runEpisode" should {
    "create 8 states in the matrix" in {
      val learner = new Learner()
      learner.runEpisode(0)
      learner.matrix.size shouldBe 9
    }
  }

  "qLearning" should {
    "print sensible results" in {
      val learner = new Learner()
      val t0 = System.currentTimeMillis()
      val result = learner.qLearning

      val justStartingMatrix = result.filter { case (state, _) =>
        state.getPossibleMoves.size == 9 && (state.state & 1 << 18) == 0
      }

      justStartingMatrix.keys.foreach { state =>
        printQValuesForState(justStartingMatrix, state)
      }

      // play game with learned matrix
      var game: MoveResult = Right(TicTacToeState.newState(p0))
      import TicTacToeState._

      while (game.isRight) {
        val state = game.right.get
        if (!result.exists { case (st, _) => st == state }) {
          println(s"Empty matrix for next move in:\n$state")
          val statesVisited = result.filterKeys(_ == state)
          println(s"Similar states visited: $statesVisited")
          println(s"Time spent: ${t0 - System.currentTimeMillis()}")
        }
        val nextMove = result(state).maxBy(_._2)._1
        printQValuesForState(result, game.right.get)
        game = game.move(nextMove)
        // opponent move
        if (game.isRight) {
          game = game.move(learner.bestNextMove(game.right.get))
        }
      }
    }
  }

  def printQValuesForState(matrix: Map[TicTacToeState, Map[Int, Double]], state: TicTacToeState): Unit = {
    println(s"printQValuesForState - $state")
    matrix(state).toSeq.sortBy(_._2).foreach { case (action, q) =>
      println(s"$action: q = $q")
    }
  }

}
