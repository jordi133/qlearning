package connectfour

import connectfour.ConnectFourState.{ActionType, StateRepr}
import qlearning._

/**
  * 7 column 6 row (row 0 is bottom row, row 5 is top row) game
  *
  * Model as 1 long:
  * - for every location (42), a 0 for p0 and a 1 for p1 (rightmost 7 bits are the bottom row, from left to right)
  * - for every column, 3 bits indicating how many tokens are put in it (21 bits)
  * - 1 bit for which player is next
  */

object ConnectFourState {
  val rows = 6
  val cols = 7
  val currentPlayerBit = 63
  val colCountOffsets = Array(42, 45, 48, 51, 54, 57, 60)

  type StateRepr = Long
  type ActionType = Int

  private val diagonals1 = Seq((-3, -3), (-2, -2), (-1, -1), (1, 1), (2, 2), (3, 3)).sliding(3).toVector
  private val diagonals2 = Seq((-3, 3), (-2, 2), (-1, 1), (1, -1), (2, -2), (3, -3)).sliding(3).toVector
  val diagonals: Vector[Seq[(Int, Int)]] = diagonals1 ++ diagonals2

  def newState: Int => ConnectFourState = { startingPlayer =>
    val state = startingPlayer.toLong << currentPlayerBit
    ConnectFourState(state)
  }
}

case class ConnectFourState private[connectfour](longState: Long) extends GameState[ConnectFourState, StateRepr, ActionType] {

  import ConnectFourState._

  val currentPlayer: PlayerId = (longState >> currentPlayerBit).toInt & 1

  def tokenAt(col: Int, row: Int): Token = {
    val tokensInCol = (longState >> colCountOffsets(col)) & 7
    if (row >= tokensInCol) {
      noToken
    } else {
      ((longState >> (row * cols + col)) & 1).toInt
    }
  }

  def tokensInCol(col: Int): Int = ((longState >> colCountOffsets(col)) & 7).toInt

  def move(col: Int): MoveResult[ConnectFourState] = {
    val tokensInThisCol = tokensInCol(col)
    require(tokensInThisCol < rows, s"Column $col already filled in \n$boardAsString")

    if (isWonByMove(col)) {
      val newState = processMoveAt(tokensInThisCol, col)
      Left((currentPlayer, ConnectFourState(newState).asInstanceOf[this.type]))
    } else if (tokensInThisCol == rows - 1) {
      val newState = processMoveAt(tokensInThisCol, col)
      Left((pDraw, ConnectFourState(newState).asInstanceOf[this.type]))
    } else {
      val newState = processMoveAt(tokensInThisCol, col)
      Right(ConnectFourState(newState).asInstanceOf[this.type])
    }
  }

  def isWonByMove(col: ActionType): Boolean = {
    val row = tokensInCol(col)
    val leftmostRelevantCol = Math.max(col - 3, 0)
    val rightMostRelevantCol = Math.min(col + 3, cols - 1)

    val quartetsOnRow = (leftmostRelevantCol to rightMostRelevantCol - 3) map { c =>
      Seq((c, row), (c + 1, row), (c + 2, row), (c + 3, row)).filter(_ != (col, row))
    }

    val quartetOnColumn =
      if (row >= 3) {
        Some(Seq((col, row - 1), (col, row - 2), (col, row - 3)))
      } else {
        None
      }

    val actualDiagonals = diagonals.map {
      _.map { case (c, r) => (c + col, r + row) }
    }
    val relevantDiagonals = actualDiagonals.filterNot {
      _.exists { case (c, r) => r < 0 || c < 0 || r >= rows || c >= cols }
    }

    val quartetsToCheck = quartetsOnRow ++ quartetOnColumn ++ relevantDiagonals

    val willConnectFour = quartetsToCheck.exists { quartet =>
      quartet.forall { case (c, r) =>
        tokenAt(c, r) == currentPlayer
      }
    }

    quartetsToCheck.nonEmpty && willConnectFour
  }

  /**
    * Updates token at (row, col) and flips the bit representing the current player
    */
  def processMoveAt(row: Int, col: Int): Long = {
    // Increase counter for nr of tokens in column
    //    println(s"processing move for:\n$boardAsString")
    val tokenCountInCol = (longState >> colCountOffsets(col)) & 7
    val tokenCountUpdated = longState & ~(7l << colCountOffsets(col)) | ((tokenCountInCol + 1l) << colCountOffsets(col))
    val index = row * cols + col
    val tokenUpdated = tokenCountUpdated + (currentPlayer.toLong << index)
    val nextPlayerUpdates = tokenUpdated ^ (1l << currentPlayerBit.toLong)
    nextPlayerUpdates
  }

  def getPossibleMoves: Seq[Int] = (0 until cols).filter(tokensInCol(_) < rows)

  def boardAsString: String = {
    val rowStrings = (0 until rows) map { r =>
      val tokens = (0 until cols).map(tokenAt(_, rows - r - 1))
      tokens.map(tokenToChar).mkString(" ")
    }
    rowStrings.mkString("\n")
  }

  override def toString: String = {
    StringBuilder.newBuilder
      .append(s"ConnectFourState($longState)\n")
      .append(tokenToChar(playerTokens(currentPlayer))).append(" to move next\n")
      .append(boardAsString)
      .toString()
  }

  /**
    * Return the state from the perspective of the current player
    */
  def pureState: Long = {
    var result = longState
    for {
      c <- 0 until cols // for every column
      t <- 0 until tokensInCol(c) // check how many tokens are in it
    } {
      // xor each bit with currentPlayerIndex
      result = result ^ (currentPlayer.toLong << t * rows + c)
    }

    // set current player bit to 0
    result & ~(1l << currentPlayerBit)
  }
}
