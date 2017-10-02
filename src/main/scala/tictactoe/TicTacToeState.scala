package tictactoe

/**
  * Created by Jordi on 30-9-2017.
  */

object TicTacToeState {
  val stateSize = 9
  val emptyState = 0

  def newState(startingPlayer: Player) = TicTacToeState(emptyState + (startingPlayer << (2 * stateSize)))

  implicit class ImplicitState(state: Either[Player, TicTacToeState]) {
    def move(index: Int): Either[Player, TicTacToeState] = state.flatMap(_.move(index))

    def asString: String = state match {
      case Left(winner) => s"Game won by $winner"
      case Right(st) => st.toString
    }
  }

}

/**
  * @param state The state of the game. Counting from the rightmost bit, the first two bits correspond
  *              to the topleft field, containing 00 for '-', 01 for 'O' and 10 for 'X'. The next two
  *              correspond to the middle field of the top row, and so on. The 18th bit form the right
  *              contains the next player
  */
case class TicTacToeState private[tictactoe](state: Int) {//} extends AnyVal {

  def tokenAt(i: Int): Token =
    (state >> (2 * i)) & 3

  lazy val currentPlayer: Player = (state >> (2 * TicTacToeState.stateSize)) & 1

  def updateTokenAt(index: Int): Int = state + (playerTokens(currentPlayer) << (2 * index))

  /**
    * Returns either Left(winning player) or Right(resulting board after move)
    *
    * @param index
    * @return
    */
  def move(index: Int): MoveResult = {
    require(index >= 0 && index < TicTacToeState.stateSize, s"Cannot place token on $index")
    require(tokenAt(index) == noToken, s"Cannot place token on $index in $this")

    if (isWonByMove(index)) {
      Left(currentPlayer)
    } else if (getPossibleMoves.size == 1) {
      Left(pDraw)
    } else {
      // Update token and flip bit representing next player
      val newIntState = updateTokenAt(index) ^ (1 << (2 * TicTacToeState.stateSize))

      // If only 1 token is on the board, normalize state by rotating TODO
      // If 2 tokens on the board, normalize by mirroring  TODO

      Right(TicTacToeState(newIntState))
    }
  }

  def forceMove(index: Int): TicTacToeState = TicTacToeState(updateTokenAt(index) ^ (1 << (2 * TicTacToeState.stateSize)))

  /**
    * Function calculates whether the game can be won by the current player
    *
    * @param index
    * @return
    */
  def isWonByMove(index: Int): Boolean = {
    // First gather all indices of the row, column and possibly diagonals that can form three-in-a-line (index to move on is filtered out)
    val row = index / 3 // 0, 1 or 2
    val rowIndices = Some(Seq(row * 3, row * 3 + 1, row * 3 + 2).filter(_ != index))

    val col = index % 3
    val colIndices = Some(Seq(col, 3 + col, 6 + col).filter(_ != index))

    // if index is on diagonal, generate indices of diagonal
    val diagonal1Indices =
      if (liesOnDiagonal1(index)) {
        Some(diagonal1.filter(_ != index))
      } else {
        None
      }
    val diagonal2Indices =
      if (liesOnDiagonal2(index)) {
        Some(diagonal2.filter(_ != index))
      } else {
        None
      }

    val triplesToCheck = rowIndices ++ colIndices ++ diagonal1Indices ++ diagonal2Indices

    triplesToCheck.exists { triple =>
      triple.forall { index =>
        tokenAt(index) == playerTokens(currentPlayer)
      }
    }
  }

  lazy val getPossibleMoves: IndexedSeq[Int] = (0 until TicTacToeState.stateSize).filter(tokenAt(_) == noToken)

  def toCharArray: IndexedSeq[Char] = for (i <- 0 until TicTacToeState.stateSize) yield tokenToChar(tokenAt(i))

  override def toString: String = {
    StringBuilder.newBuilder
      .append(s"TicTacToeState($state)")
      .append(tokenToChar(playerTokens(currentPlayer))).append(" to move next\n")
      .append(toCharArray.grouped(3).mkString("\n"))
      .toString()
  }
}
