package connectfour

import scala.io.StdIn
import scala.util.{Failure, Random, Success, Try}

import qlearning._

object Main extends App {

  override def main(args: Array[String]): Unit = {
    val seed = Random.nextInt()
    val t0 = System.currentTimeMillis()
    val rnd = new Random(seed)
    println(s"Training opponent with seed $seed")
    val learner = new QLearner[Long, ConnectFourState](learningRate = 0.2d, discountFactor = 0.5d, episodes = 100000, seed = rnd.nextInt())
    val qMatrix = learner.qLearning(startingPlayer => ConnectFourState.newState(startingPlayer))
    val opponent = new TrainedPlayer[Long, ConnectFourState](qMatrix, rnd.nextInt())
    println(s"Finished training (took ${System.currentTimeMillis() - t0} ms)")

    var stop = false
    while (!stop) {
      val startingPlayer = rnd.nextInt(2)
      val newGame = ConnectFourState.newState(startingPlayer)
      val winner = playGame(Right(newGame), opponent)
      if (winner == p0) {
        println("You won")
      } else if (winner == pDraw) {
        println("Draw")
      } else {
        println("You lost")
      }
      println("Player another game (y/[n])?")
      stop = StdIn.readChar() != 'y'
    }
  }

  def readIndex(possibleMoves: Seq[Int]): Int = {
    println(s"Enter any index to place next token (possibilities: ${possibleMoves.mkString(", ")})")
    Try(StdIn.readLine().toInt) match {
      case Success(index) if possibleMoves.contains(index) =>
        index
      case Success(index) =>
        println(s"$index not one of ${possibleMoves.mkString(", ")}")
        readIndex(possibleMoves)
      case Failure(ex) =>
        println(s"${ex.toString} - try again}")
        readIndex(possibleMoves)
    }
  }

  def playGame(mr: MoveResult[ConnectFourState], opponent: TrainedPlayer[_, ConnectFourState]): PlayerId = mr match {
    case Left((winner, _)) =>
      winner
    case Right(state) if state.currentPlayer == p0 =>
      println(state.toString)
      val index = readIndex(state.getPossibleMoves)
      val newState = state.move(index)
      playGame(newState, opponent)
    case Right(state) =>
      println(state.toString)
      val index = opponent.getNextMove(state)
      val newState = state.move(index)
      playGame(newState, opponent)
  }
}
