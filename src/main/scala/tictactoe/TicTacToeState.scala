package tictactoe

import qlearning._

/**
  * Created by Jordi on 30-9-2017.
  */

object TicTacToeState {
  val stateSize = 9
  val emptyState = 0
  val currentPlayerBit: Int = 2 * stateSize
  val diagonal1 = Seq(0, 4, 8)
  val diagonal2 = Seq(2, 4, 6)

  def newState: Int => TicTacToeState = { startingPlayer =>
    TicTacToeState(emptyState + (startingPlayer << currentPlayerBit))
  }

}

/**
  * @param state The state of the game. Counting from the rightmost bit, the first nine bits represent
  *              whether a field is filled, and the second nine bits represent the token that is placed
  *              if it is filled. The next bit represents the next player.
  *
  *              Extending AnyVal seems slightly faster (for now)
  */
case class TicTacToeState private[tictactoe](state: Int) extends GameState[TicTacToeState, Int] {

  import TicTacToeState.{currentPlayerBit, stateSize, diagonal1, diagonal2}

  def tokenAt(i: Int): Token =
    if ((state >> i & 1) == 0) noToken
    else (state >> (stateSize + i)) & 1

  lazy val currentPlayer: PlayerId = (state >> currentPlayerBit) & 1

  private def updateTokenAt(index: Int): Int = state & ~(1 << (stateSize + index)) | (playerTokens(currentPlayer) << (stateSize + index)) | (1 << index)

  /**
    * Returns either Left(winning player) or Right(resulting board after move)
    *
    * @param index
    * @return
    */
  def move(index: Int): MoveResult[TicTacToeState] = {
    require(index >= 0 && index < stateSize, s"Cannot place token on $index")
    require(tokenAt(index) == noToken, s"Cannot place token on $index in $this")

    if (isWonByMove(index)) {
      Left((currentPlayer, forceMove(index)))
    } else if (getPossibleMoves.size == 1) {
      Left(pDraw, forceMove(index))
    } else {
      // Update token and flip bit representing next player
      val newIntState = updateTokenAt(index) ^ (1 << currentPlayerBit)

      Right(TicTacToeState(newIntState))
    }
  }

  /**
    * Performs a move without checking whether game will be won or whether a token is overrwritten
    */
  def forceMove(index: Int): TicTacToeState = TicTacToeState(updateTokenAt(index) ^ (1 << currentPlayerBit))

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
      if (diagonal1.contains(index)) {
        Some(diagonal1.filter(_ != index))
      } else {
        None
      }
    val diagonal2Indices =
      if (diagonal2.contains(index)) {
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

  lazy val getPossibleMoves: IndexedSeq[Int] = (0 until stateSize).filter(tokenAt(_) == noToken)

  override def toString: String = {
    StringBuilder.newBuilder
      .append(s"TicTacToeState($state)\n")
      .append(tokenToChar(playerTokens(currentPlayer))).append(" to move next\n")
      .append(boardAsString)
      .toString()
  }

  def boardAsString: String = toCharArray.grouped(3).mkString("\n")

  def toCharArray: IndexedSeq[Char] = for (i <- 0 until stateSize) yield tokenToChar(tokenAt(i))

  /**
    * Return the state from the perspective of the current player: locations with own token are marked '01' and locations with opponents token
    * are marked '10'. This facilitates learning from both sides of a certain game
    * TODO: normalize state so that type cardinality is minimized by rotating and mirroring the board
    *
    * @return
    */
  lazy val pureState: Int = {
    if (currentPlayer == p0) {
      state
    }
    else {
      var result = 0
      for (i <- 0 until stateSize) {
        tokenAt(i) match {
          case `noToken` =>
          case `p0Token` => result += (p1Token << (stateSize + i)) | (1 << i)
          case `p1Token` => result += (p0Token << (stateSize + i)) | (1 << i)
        }
      }
      result
    }
  }
}
