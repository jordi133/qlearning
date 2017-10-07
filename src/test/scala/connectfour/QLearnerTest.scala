package connectfour

import org.scalatest.{Matchers, WordSpec}

class QLearnerTest extends WordSpec with Matchers {

  "qLearning" should {
    val learner = new QLearner(learningRate = 0.5d, discountFactor = 0.8d, episodes = 1000000, seed = 0)
    val t0 = System.currentTimeMillis()
    val result = learner.qLearning
    val dt =System.currentTimeMillis() - t0
    "give highest Q Value to the move preventing the opponent from winning" in {
      println(s"training took $dt ms")

      val state = ConnectFourState.newState(p0)
        .move(0) // - - - - - - -
        .move(1) // - - - - - - -
        .move(2) // - - - - - - -
        .move(1) // - X - - - - -
        .move(0) // O X - - - - -
        .move(1) // O X O - - - -
        .right.get

      val indexToPreventWin = 1

      val otherIndices = Seq(0, 2, 3, 4, 5, 6)

      println("give highest Q Value to the move preventing the opponent from winning")
      result(state.pureState).foreach{case (col, q) => println(s"col $col: q=$q")}

      otherIndices.foreach { i =>
        result(state.pureState)(i) should be < result(state.pureState)(indexToPreventWin)
      }
    }
    "Give highest Q Value to winning move" in {
      println(s"training took $dt ms")

      val state = ConnectFourState.newState(p0)
        .move(0) // - - - - - - -
        .move(1) // - - - - - - -
        .move(0) // - - - - - - -
        .move(1) // O X - - - - -
        .move(0) // O X - - - - -
        .move(1) // O X - - - - -
        .right.get
      val winningMove = 0
      val otherIndices = 1 until ConnectFourState.cols

      println("Give highest Q Value to winning move")
      result(state.pureState).foreach{case (col, q) => println(s"col $col: q=$q")}

      otherIndices.foreach { i =>
        result(state.pureState)(i) should be < result(state.pureState)(winningMove)
      }

    }
  }
}
