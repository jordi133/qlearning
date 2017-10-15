package connectfour

import scala.io.StdIn
import scala.util.{Failure, Random, Success, Try}

import qlearning._

object Main extends App {

  override def main(args: Array[String]): Unit = {
    val seed = Random.nextInt()
    val rnd = new Random(seed)

    val filename = "1000000-eps0.5-lr0.8-df.cfql"

    val loadedOpponent =
//      if (args.length == 0) {
        loadOpponentFromFile(filename)
//      } else {
//        Failure(new IllegalArgumentException(s"Could not read player from file ${args.head}"))
//      }

    val opponent = loadedOpponent match {
      case Success(player) =>
        player
      case Failure(cause) =>
        println(cause.getMessage)
        println(s"Training opponent with seed $seed")
        trainOpponent(rnd)
    }

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

//  def askNextAction

  def loadOpponentFromFile(filename: String): Try[TrainedPlayer[Long, Int, ConnectFourState]] = {
    val matrix = Learner.readMatrixFromFile(filename).withFilter(_.nonEmpty)
    val player = matrix.map(m => new TrainedPlayer[Long, Int, ConnectFourState](m, 0))
    player
  }

  def trainOpponent(rnd: Random): TrainedPlayer[Long, Int, ConnectFourState] = {
    val t0 = System.currentTimeMillis()
    val learner = new QLearner[Long, Int, ConnectFourState](ConnectFourState.newState, learningRate = 0.2d, discountFactor = 0.8d, episodes = 10000, seed = rnd.nextInt())
    val qMatrix: QMatrix[Long, Int] = learner.qLearning()
    val opponent = new TrainedPlayer[Long, Int, ConnectFourState](qMatrix, rnd.nextInt())
    println(s"Finished training (took ${System.currentTimeMillis() - t0} ms)")
    opponent
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

  def playGame(mr: MoveResult[ConnectFourState], opponent: TrainedPlayer[_, Int, ConnectFourState]): PlayerId = mr match {
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
