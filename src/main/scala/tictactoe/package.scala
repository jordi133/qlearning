/**
  * Created by Jordi on 30-9-2017.
  */
package object tictactoe {

  type Player = Int
  // either Left(winning player) or Right(resulting board after move)
  type MoveResult = Either[Player, TicTacToeState]

  type Token = Int

  val players = List(0, 1)
  val p0Token = 1
  val p1Token = 2
  val noToken = 0
  val p0TokenChar = 'O'
  val p1TokenChar = 'X'
  val noTokenChar = '-'
  val playerTokens = Map(0 -> p0Token, 1 -> p1Token)
  val tokenToChar = Map(p0Token -> p0TokenChar, p1Token -> p1TokenChar, noToken -> noTokenChar)

  val diagonal1 = Seq(0, 4, 8)
  val diagonal2 = Seq(2, 4, 6)

  def liesOnDiagonal1(index: Int): Boolean = diagonal1.contains(index)

  def liesOnDiagonal2(index: Int): Boolean = diagonal2.contains(index)


}
