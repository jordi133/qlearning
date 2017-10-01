package tictactoe

/**
  * Created by Jordi on 30-9-2017.
  */
class Learner {


  def bruteForce(currentState: TicTacToeState, stateValueMap: Map[(TicTacToeState, Int), Double] = Map.empty.withDefault(_ => 0)) = {

    // get possible next moves
    val nextMoves = currentState.getPossibleMoves

    for (move <- nextMoves) {
      currentState.move(move) match {
        case Left(player) => // process game won by player, combination of state/move has high value
        case Right(state) => // state neutral, recursive call with next state
      }
    }


  }



  def doRandomMoveForOpponent(state: TicTacToeState) = ???

}
