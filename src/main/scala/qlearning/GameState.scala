package qlearning

trait GameState[G, S] {

  def currentPlayer: PlayerId

  def pureState: S

  def move(action: Action): MoveResult[G]

  def isWonByMove(action: Action): Boolean

  def getPossibleMoves: Seq[Action]

  def boardAsString: String

}
