package object qlearning {
  type PlayerId = Int

  type Action = Int

  type QMatrix = Map[PureState, Map[Action, Double]]

  type Token = Int

  type PureState = Long

  val pDraw: Int = -1
  val p0: PlayerId = 0
  val p1: PlayerId = 1
  val players = List(p0, p1)
  val noToken: Token = -1
  val p0Token: Token = 0
  val p1Token: Token = 1
  val p0TokenChar: Char = 'O'
  val p1TokenChar: Char = 'X'
  val noTokenChar: Char = '-'
  val playerTokens: Map[PlayerId, Token] = Map(p0 -> p0Token, p1 -> p1Token)
  val tokenToChar: Map[Token, Char] = Map(p0Token -> p0TokenChar, p1Token -> p1TokenChar, noToken -> noTokenChar)

}
