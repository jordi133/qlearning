package tictactoe

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by Jordi on 30-9-2017.
  */
class TicTacToeStateTest extends WordSpec with Matchers {

  "tokenAt" should {
    "work for empty board" in {
      val tokens = Vector.fill(TicTacToeState.stateSize)(noTokenChar)
      val v = TicTacToeState.stateAsInt(tokens, 0)
      val state = TicTacToeState(v)
      (0 until TicTacToeState.stateSize).foreach {
        state.tokenAt(_) shouldBe noToken
      }
    }
    "work for filled board p0" in {
      val tokens = Vector.fill(TicTacToeState.stateSize)(p0TokenChar)
      val v = TicTacToeState.stateAsInt(tokens, 0)
      val state = TicTacToeState(v)
      (0 until TicTacToeState.stateSize).foreach {
        state.tokenAt(_) shouldBe p0Token
      }
    }
    "work for filled board p1" in {
      val tokens = Vector.fill(TicTacToeState.stateSize)(p1TokenChar)
      val v = TicTacToeState.stateAsInt(tokens, 0)
      val state = TicTacToeState(v)
      (0 until TicTacToeState.stateSize).foreach {
        state.tokenAt(_) shouldBe p1Token
      }
    }
  }

  "move" should {
    "return an updated board after normal move" in {
      val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar))
      val state = TicTacToeState(v)
      state.move(2) match {
        case Right(newState) => newState.tokenAt(2) shouldBe p0Token
        case Left(_) => fail
      }
      println(state.move(2).flatMap(_.move(1)).flatMap(_.move(4)))
    }
    "throw IndexOutOfBoundsException when target index is out of bounds (too low)" in {
      intercept[IllegalArgumentException] {
        val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar))
        val state = TicTacToeState(v)
        state.move(-1)
      }
    }
    "throw IndexOutOfBoundsException when target index is out of bounds (too high)" in {
      intercept[IllegalArgumentException] {
        val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar))
        val state = TicTacToeState(v)
        state.move(9)
      }
    }
    "throw IllegalArgumentException when target index is taken already" in {
      intercept[IllegalArgumentException] {
        val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar))
        val state = TicTacToeState(v)
        state.move(0)
      }
    }
    "return winning player of game is won by this move" in {
      val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, p0TokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar))
      val state = TicTacToeState(v)

      state.move(2) shouldBe Left(0)
    }
  }


  "isWonByMove" should {
    "return true if won by next move winning with row" in {
      val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, p0TokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar))
      val state = TicTacToeState(v)

      state.isWonByMove(2) shouldBe true
    }
    "return false if won not by next move winning with row" in {
      val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, p0TokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar), currentPlayer = 1)
      val state = TicTacToeState(v)

      state.isWonByMove(2) shouldBe false
    }
    "return true if won by next move winning with column" in {
      val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, noTokenChar, noTokenChar, p0TokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar))
      val state = TicTacToeState(v)

      state.isWonByMove(6) shouldBe true
    }
    "return false if won not by next move winning with column" in {
      val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, noTokenChar, noTokenChar, p0TokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar), currentPlayer = 1)
      val state = TicTacToeState(v)

      state.isWonByMove(6) shouldBe false
    }
    "return true if won by next move winning with diagonal" in {
      val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, noTokenChar, noTokenChar, noTokenChar, p0TokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar))
      val state = TicTacToeState(v)

      state.isWonByMove(8) shouldBe true
    }
    "return false if won not by next move winning with diagonal" in {
      val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, noTokenChar, noTokenChar, noTokenChar, p0TokenChar, noTokenChar, noTokenChar, noTokenChar, noTokenChar), currentPlayer = 1)
      val state = TicTacToeState(v)

      state.isWonByMove(8) shouldBe false
    }
  }

  "possibleMoves" should {
    "return empty seq on full board" in {
      val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, p0TokenChar, p0TokenChar, p0TokenChar, p0TokenChar, p0TokenChar, p0TokenChar, p0TokenChar, p0TokenChar))
      val state = TicTacToeState(v)

      state.getPossibleMoves shouldBe empty
    }
    "return open indices in gamestate" in {
      val v = TicTacToeState.stateAsInt(Vector(p0TokenChar, noTokenChar, noTokenChar, p0TokenChar, noTokenChar, p0TokenChar, p0TokenChar, p0TokenChar, noTokenChar))
      val state = TicTacToeState(v)

      state.getPossibleMoves shouldBe Seq(1, 2, 4, 8)
    }
  }
}
