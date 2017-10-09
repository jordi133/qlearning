package connectfour

import qlearning.MoveResult

object MoveResultImplicit {

  implicit class ImplicitState(state: MoveResult[ConnectFourState]) {
    def move(index: Int): MoveResult[ConnectFourState] = state.flatMap(_.move(index))

    def pureState: Long = state match {
      case Left((player, _)) => player
      case Right(st) => st.pureState
    }

    def asString: String = state match {
      case Left((winner, board)) => s"Game won by $winner\n${board.boardAsString}"
      case Right(st) => st.toString
    }
  }
}
