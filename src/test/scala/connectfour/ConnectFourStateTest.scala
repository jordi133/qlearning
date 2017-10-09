package connectfour

import org.scalatest.{Matchers, WordSpec}

import ConnectFourState._
import qlearning._

class ConnectFourStateTest extends WordSpec with Matchers {


  "tokensInCol" should {
    "return 0 for every new game started by p0" in {
      val state = ConnectFourState.newState(p0)

      (0 until cols).foreach { c =>
        state.tokensInCol(c) shouldBe 0
      }
    }
    "return 0 for every new game started by p1" in {
      val state = ConnectFourState.newState(p1)

      (0 until cols).foreach { c =>
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
        state.tokenAt(c, r) shouldBe noToken
      }
      state.tokensInCol(0) shouldBe 0
    }
    "return an updated token" in {
      val state = ConnectFourState(newState(p0).processMoveAt(0, 0))

      state.tokenAt(0, 0) shouldBe p0Token
      state.tokensInCol(0) shouldBe 1
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
        .flatMap(_.move(3))
        .flatMap(_.move(3))
        .flatMap(_.move(5))

      state.right.get.pureState shouldBe state.right.get.longState
    }
    "return the same state if it is already pure 2" in {
      val state = ConnectFourState.newState(p1)
        .move(0)
        .flatMap(_.move(3))
        .flatMap(_.move(5))
        .flatMap(_.move(3))
        .flatMap(_.move(1))

      state.right.get.pureState shouldBe state.right.get.longState
    }
    "return the same state if the same moves have been played, regarless of starting player" in {
      val s0 = ConnectFourState.newState(p0)
        .move(0)
        .flatMap(_.move(1))
      val s1 = ConnectFourState.newState(p1)
        .move(0)
        .flatMap(_.move(1))

      s0.right.get.pureState shouldBe s1.right.get.pureState

    }
  }

  "isWonByMove" should {
    "return false when not winning" in {
      val state = ConnectFourState.newState(p1)
        .move(0)
        .flatMap(_.move(3))
        .flatMap(_.move(5))
        .flatMap(_.move(3))
        .flatMap(_.move(1))

      state.right.get.isWonByMove(0) shouldBe false
    }
    "return true when winning 0" in {
      val state = ConnectFourState.newState(p1)
        .move(0)
        .flatMap(_.move(1))
        .flatMap(_.move(0))
        .flatMap(_.move(1))
        .flatMap(_.move(0))
        .flatMap(_.move(1))

      state.right.get.isWonByMove(0) shouldBe true
    }
    "return true when winning 1" in {
      val state = ConnectFourState.newState(p1)
        .move(0)
        .flatMap(_.move(4))
        .flatMap(_.move(1))
        .flatMap(_.move(4))
        .flatMap(_.move(2))
        .flatMap(_.move(4))

      state.right.get.isWonByMove(3) shouldBe true
    }
    "return true when winning 2" in {
      val state = ConnectFourState.newState(p1)
        .move(0) // - - - - - - -
        .flatMap(_.move(1)) // - - - - - - -
        .flatMap(_.move(1)) // - - - - - - -
        .flatMap(_.move(2)) // - X X X - - -
        .flatMap(_.move(2)) // X O O O - - -
        .flatMap(_.move(3))
        .flatMap(_.move(3))

      state.right.get.isWonByMove(4) shouldBe true
    }
    "return true when winning 3" in {
      val state = ConnectFourState.newState(p1)
        .move(0) // - - - - - - -
        .flatMap(_.move(1)) // - - - - - - -
        .flatMap(_.move(1)) // - - X X - - -
        .flatMap(_.move(2)) // - X X O - - -
        .flatMap(_.move(2)) // X O O O - O -
        .flatMap(_.move(3))
        .flatMap(_.move(2))
        .flatMap(_.move(3))
        .flatMap(_.move(3))
        .flatMap(_.move(5))

      state.right.get.isWonByMove(3) shouldBe true
    }
  }
}
