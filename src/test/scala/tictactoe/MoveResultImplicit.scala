package tictactoe

import qlearning.MoveResult

object MoveResultImplicit {

  implicit class ImplicitState(state: MoveResult[TicTacToeState]) {
    def move(index: Int): MoveResult[TicTacToeState] = state.flatMap(_.move(index))

    def pureState: Int = state match {
      case Left((player, _)) => player
      case Right(st) => st.pureState
    }

    def asString: String = state match {
      case Left((winner, board)) => s"Game won by $winner\n${board.boardAsString}"
      case Right(st) => st.toString
    }
  }

}
