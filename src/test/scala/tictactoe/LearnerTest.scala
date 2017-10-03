package tictactoe

import org.scalatest.{Matchers, WordSpec}

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

      // TODO Find out why results first seem to converge to logical values (after 2500 games), and then diverge (at 250000)
      /* values for 2500 (makes sense)
        1: q = 0.0019225771224361293
        5: q = 0.003051954486031257
        3: q = 0.003402578503244139
        7: q = 0.00603886557613797
        2: q = 0.006125497413798682
        0: q = 0.009226984509917557
        8: q = 0.011698244259870906
        6: q = 0.012377098126872032
        4: q = 0.026976876478617805
       */

      /* values for 250000
        6: q = 1.9286373122506508E-4
        7: q = 0.0013930842042264087
        5: q = 0.0027989270242425443
        1: q = 0.003442113006935252
        4: q = 0.010663146489418742
        8: q = 0.012041978885223388
        2: q = 0.01266732219151288
        3: q = 0.012694239613117447
        0: q = 0.025792897562616622
       */
      val justStartingMatrix = result.filter { case (state, _) =>
        state.getPossibleMoves.size == 9 && (state.state & 1 << 18) == 0
      }

      justStartingMatrix
        .values.flatten
        .toSeq.sortBy(_._2)
        .foreach { case (action, q) =>
          println(s"$action: q = $q")
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
