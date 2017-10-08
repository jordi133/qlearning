package tictactoe

import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

class TrainedConnectFourPlayerTest  extends WordSpec with Matchers {

  "getNextBestMove" should {
    "Pick action with highest Q Value" in {
      val state = TicTacToeState.newState(p0)
        .move(0) // O - O
        .move(3) // X - -
        .move(2) // - - -
        .right.get

      val qMatrix: QMatrix = Map(state.pureState -> Map[Action, Double](1 -> 1, 4 -> 0, 5 -> -1, 6 -> 0, 7 -> 0.5, 8 -> 0.25))

      TrainedPlayer.getNextBestMove(state, qMatrix, new Random(0)) shouldBe 1
    }
  }
}
