package qlearning

trait GameState[G, PureState, Action] {

  def currentPlayer: PlayerId

  def pureState: PureState

  def move(action: Action): MoveResult[G]

  def isWonByMove(action: Action): Boolean

  def getPossibleMoves: Seq[Action]

  def boardAsString: String

}
