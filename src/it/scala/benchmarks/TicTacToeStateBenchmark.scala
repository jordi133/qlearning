package benchmarks

import org.scalameter.{Bench, Gen}
import tictactoe.TicTacToeState

import scala.util.Random

/**
  * Created by Jordi on 1-10-2017.
  */

object TicTacToeStateBenchmark extends Bench.ForkedTime {

  measure method "map" in {
    using(Gen.range("size")(100000, 100000, 100000)) curve ("Nr of games") in { _ =>
      playFullGame
    }
  }

  val seed = 0
  val random = new Random(seed)

  def playFullGame: Int = {
    def playUntilWin(state: TicTacToeState): Int = {
      val nextMoves = state.getPossibleMoves
      val nextMove = nextMoves(random.nextInt(nextMoves.size))
      state.move(nextMove) match {
        case Left((winner, _)) => winner
        case Right(newState) => playUntilWin(newState)
      }
    }

    val state = TicTacToeState.newState(random.nextInt(2))
    playUntilWin(state)
  }
}
