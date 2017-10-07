package benchmarks

import connectfour.ConnectFourState
import org.scalameter.{Bench, Gen}

import scala.util.Random

object ConnectFourBenchmark extends Bench.ForkedTime {

  measure method "map" in {
    using(Gen.range("size")(100000, 100000, 100000)) curve ("Nr of games") in { _ =>
      playFullGame
    }
  }

  val seed = 0
  val random = new Random(seed)

  def playFullGame: Int = {
    def playUntilWin(state: ConnectFourState): Int = {
      val nextMoves = state.getPossibleMoves
      val nextMove = nextMoves(random.nextInt(nextMoves.size))
      state.move(nextMove) match {
        case Left((winner, _)) => winner
        case Right(newState) => playUntilWin(newState)
      }
    }

    val state = ConnectFourState.newState(random.nextInt(2))
    playUntilWin(state)
  }
}
