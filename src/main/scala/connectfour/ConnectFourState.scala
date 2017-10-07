package connectfour

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

  def newState(startingPlayer: PlayerId): ConnectFourState = {
    val state = startingPlayer.toLong << currentPlayerBit
    for (c <- 0 until cols) println(s"Tokens in col $c: ${((state >> colCountOffsets(c)) & 7).toInt}")

    ConnectFourState(state)
  }

  implicit class ImplicitState(state: MoveResult) {
    def move(index: Int): MoveResult = state.flatMap(_.move(index))

    def pureState: Long = state match {
      case Left((player, _)) => player
      case Right(st) => st.pureState
    }

    def asString: String = state match {
      case Left((winner, board)) => s"Game won by $winner\n${board.boardAsString}"
      case Right(st) => st.toString
    }

    def longState: Long = state match {
      case Left((_, st)) => st.longState
      case Right(st) => st.longState
    }
  }

}

case class ConnectFourState private[connectfour](longState: Long) {

  import ConnectFourState._

  val currentPlayer: PlayerId = (longState >> currentPlayerBit).toInt & 1

  def tokenAt(row: Int, col: Int): Token = {
    val tokensInCol = (longState >> colCountOffsets(col)) & 7
    if (row >= tokensInCol) {
      noToken
    } else {
      ((longState >> (row * cols + col)) & 1).toInt
    }
  }

  def tokensInCol(col: Int): Int = ((longState >> colCountOffsets(col)) & 7).toInt

  def move(col: Int): MoveResult = {
    val tokensInThisCol = tokensInCol(col)
    require(tokensInThisCol < rows, s"Column $col already filled in \n$boardAsString")
    // check for possible win
    // TODO

    //
    val newState = processMoveAt(tokensInThisCol, col)
    Right(ConnectFourState(newState))
  }

  /**
    * Updates token at (row, col) and flips the bit representing the current player
    */
  def processMoveAt(row: Int, col: Int): Long = {
    // Increase counter for nr of tokens in column
    println(s"processing move for:\n$boardAsString")
    val tokenCountInCol = (longState >> colCountOffsets(col)) & 7
    val tokenCountUpdated = longState & ~(7l << colCountOffsets(col)) | ((tokenCountInCol + 1l) << colCountOffsets(col))
    val index = row * cols + col
    val tokenUpdated = tokenCountUpdated + (currentPlayer.toLong << index)
    val nextPlayerUpdates = tokenUpdated ^ (1l << currentPlayerBit.toLong)
    nextPlayerUpdates
  }

  def boardAsString: String = {
    val rowStrings = (0 until rows) map { r =>
      val tokens = (0 until cols).map(tokenAt(rows - r - 1, _))
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
      result = result ^ (1l << t * rows + c)
    }

    // set current player bit to 0
    result & ~(1l << currentPlayerBit)
  }
}
