package qlearning

trait GameState[T] {

  type MoveResult = Either[(PlayerId, this.type), this.type]

  def currentPlayer: PlayerId

  def pureState: T

  def move(action: Action): MoveResult

  def isWonByMove(action: Action): Boolean

  def getPossibleMoves: Seq[Action]

  def boardAsString: String

  implicit class ImplicitState(state: MoveResult) {
//    def move(index: Action): MoveResult = state.flatMap(_.move(index))
    def move(index: Action): MoveResult =
      state match {
        case Right(st) => st.move(index)
        case left =>  left
      }


    def pureState: T = state match {
//      case Left((player, _)) => player
      case Left((_, st)) => st.pureState
      case Right(st) => st.pureState
    }

    def asString: String = state match {
      case Left((winner, board)) => s"Game won by $winner\n${board.boardAsString}"
      case Right(st) => st.toString
    }

    def longState: T = state match {
      case Left((_, st)) => st.pureState
      case Right(st) => st.pureState
//      case Left((_, st)) => st.longState
//      case Right(st) => st.longState
    }
  }


}
