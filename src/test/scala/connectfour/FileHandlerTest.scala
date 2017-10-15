package connectfour

import org.scalatest.{Matchers, WordSpec}
import qlearning.QLearner

import scala.util.{Failure, Success}

class FileHandlerTest extends WordSpec with Matchers {

  "Learner" should {
    "return the same matrix after saving to and reading form file" in {
      val episodes = 10000000
      val lr = 0.5
      val df = 0.8
      val filename = s"$episodes-eps$lr-lr$df-df.cfql"
      val learner = new QLearner[Long, Int, ConnectFourState](ConnectFourState.newState, learningRate = lr, discountFactor = df, episodes = episodes, seed = 0)
      val matrix = learner.qLearning()
      FileHandler.writeMatrixToFile(matrix, filename) match {
        case Failure(cause) =>
          cause.printStackTrace()
          fail()
        case _ =>
      }
      FileHandler.readMatrixFromFile(filename) match {
        case Success(result) =>
          result shouldBe matrix
        case Failure(cause) =>
          cause.printStackTrace()
          fail()
      }
    }

    "be able to read from file" ignore {
      val matrix = FileHandler.readMatrixFromFile("1000000-eps0.5-lr0.8-df.cfql")

      matrix match {
        case Success(result) =>
          assert(result.nonEmpty)
        case Failure(cause) =>
          cause.printStackTrace()
          fail()
      }
    }
  }
}
