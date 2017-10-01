package tictactoe

/**
  * Created by Jordi on 30-9-2017.
  */

object TicTacToeState {
  val stateSize = 9

  val emptyState = 0
  val emptyBoard = Vector.fill[Char](stateSize)(noTokenChar)

  def newWithStartingPlayer(player: Player) = TicTacToeState(emptyState + (player << (2 * stateSize)))

  val tokenval = Map(p0TokenChar -> 1, p1TokenChar -> 2, noTokenChar -> 0)

  def stateFromInt(state: Int): TicTacToeState = {
    var result = emptyBoard
    for (i <- result.indices) {

      (state >> (2 * i)) & 3 match {
        case 0 => result = result.updated(i, noTokenChar)
        case 1 => result = result.updated(i, p0TokenChar)
        case 2 => result = result.updated(i, p1TokenChar)
      }
    }
    TicTacToeState(state)
  }

  // Legacy
  def stateAsInt(state: Vector[Char], currentPlayer: Int = 0) = {
    var result: Int = 0
    for (i <- 0 until stateSize) {
      val v = tokenval(state(i)) << (2 * i)
      result += v
    }
    result = result + (currentPlayer << (2 * stateSize))
    result
  }

  
}

case class TicTacToeState private[tictactoe](state: Int) extends AnyVal {

  def tokenAt(i: Int): Token =
    (state >> (2 * i)) & 3

  def currentPlayer: Player = state >> (2 * TicTacToeState.stateSize)

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
    } else {
      val nextPlayer = 1 - currentPlayer
      val newIntState = updateTokenAt(index) + (nextPlayer << (2 * TicTacToeState.stateSize))
      Right(TicTacToeState.stateFromInt(newIntState))
    }
  }

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

  def getPossibleMoves: IndexedSeq[Int] = (0 until TicTacToeState.stateSize).filter(tokenAt(_) == noToken)


  def toCharArray: IndexedSeq[Char] = for (i <- 0 until TicTacToeState.stateSize) yield tokenToChar(tokenAt(i))

  override def toString: String = {
    StringBuilder.newBuilder
    .append(tokenToChar(currentPlayer)).append(" to move next\n")
      .append(toCharArray.grouped(3).mkString("\n"))
      .toString()
  }
}
