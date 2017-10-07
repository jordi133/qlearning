package connectfour

import org.scalatest.{Matchers, WordSpec}

class ConnectFourStateTest extends WordSpec with Matchers {

  import ConnectFourState._

  "tokensInCol" should {
    "return 0 for every new game started by p0" in {
      val state = ConnectFourState.newState(p0)

      (0 until cols).foreach { c =>
        state.tokensInCol(c) shouldBe 0
      }
    }
    "return 0 for every new game started by p1" in {
      val state = ConnectFourState.newState(p1)
      println(s"Current player: ${state.currentPlayer}")

      (0 until cols).foreach { c =>
        println(s"checking c=$c")
        state.tokensInCol(c) shouldBe 0
      }
    }
  }

  "tokenAt" should {
    "return no token in empty state" in {
      val state = ConnectFourState.newState(p0)

      for {
        r <- 0 until rows
        c <- 0 until cols
      } {
        state.tokenAt(r, c) shouldBe noToken
      }
      state.tokensInCol(0) shouldBe 0
    }
    "return an updated token" in {
      val state = ConnectFourState(newState(p0).processMoveAt(0, 0))

      println(s"state: $state")
      state.tokenAt(0, 0) shouldBe p0Token
      state.tokensInCol(0) shouldBe 1

      println(state.boardAsString)
    }
  }

  "pureState" should {
    "return the same state if it is already pure 0" in {
      val state = ConnectFourState.newState(p0)

      state.pureState shouldBe state.longState
    }
    "return the same state if it is already pure 1" in {
      val state = ConnectFourState.newState(p0)
        .move(0)
        .move(3)
        .move(5)
        .move(3)

      state.pureState shouldBe state.longState
    }
    "return the same state if it is already pure 2" in {
      val state = ConnectFourState.newState(p1)
        .move(0)
        .move(3)
        .move(5)
        .move(3)
        .move(1)

      state.pureState shouldBe state.longState
    }
  }
}
