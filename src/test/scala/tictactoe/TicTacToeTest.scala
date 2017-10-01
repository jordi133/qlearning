package tictactoe

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by Jordi on 30-9-2017.
  */
class TicTacToeTest extends WordSpec with Matchers {

  "package object" should {
    "check for diagonal 1" in {
      liesOnDiagonal1(0) should be(true)
      liesOnDiagonal1(1) should be(false)
      liesOnDiagonal1(2) should be(false)
      liesOnDiagonal1(3) should be(false)
      liesOnDiagonal1(4) should be(true)
      liesOnDiagonal1(5) should be(false)
      liesOnDiagonal1(6) should be(false)
      liesOnDiagonal1(7) should be(false)
      liesOnDiagonal1(8) should be(true)
      liesOnDiagonal1(9) should be(false)
    }
    "check for diagonal 2" in {
      liesOnDiagonal2(0) should be(false)
      liesOnDiagonal2(1) should be(false)
      liesOnDiagonal2(2) should be(true)
      liesOnDiagonal2(3) should be(false)
      liesOnDiagonal2(4) should be(true)
      liesOnDiagonal2(5) should be(false)
      liesOnDiagonal2(6) should be(true)
      liesOnDiagonal2(7) should be(false)
      liesOnDiagonal2(8) should be(false)
    }
  }
}
