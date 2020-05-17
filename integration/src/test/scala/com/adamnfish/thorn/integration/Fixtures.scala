package com.adamnfish.thorn.integration

import com.adamnfish.thorn.Thorn
import com.adamnfish.thorn.attempt.Attempt
import com.adamnfish.thorn.logic.Games
import com.adamnfish.thorn.models._

import scala.concurrent.ExecutionContext


object Fixtures {
  val createGameRequest = CreateGame("creator name", "game name")
  val creatorAddress = PlayerAddress("creator-address")
  val player1Address = PlayerAddress("player-1-address")
  val player2Address = PlayerAddress("player-2-address")

  def createGame(context: PlayerAddress => Context)(implicit ec: ExecutionContext): Attempt[Response[Welcome]] = {
    Thorn.createGame(
      createGameRequest,
      context(creatorAddress)
    )
  }

  def joinGame(creatorWelcome: Welcome, context: PlayerAddress => Context)(implicit ec: ExecutionContext): Attempt[Response[Welcome]] = {
    Thorn.joinGame(
      JoinGame(
        Games.gameCode(creatorWelcome.gameId),
        "player 1"
      ), context(player1Address)
    )
  }

  def joinGame2(creatorWelcome: Welcome, context: PlayerAddress => Context)(implicit ec: ExecutionContext): Attempt[Response[Welcome]] = {
    Thorn.joinGame(
      JoinGame(
        Games.gameCode(creatorWelcome.gameId),
        "player 2"
      ), context(player2Address)
    )
  }

  def startGame(creatorWelcome: Welcome, playerOrder: List[Welcome], context: PlayerAddress => Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    Thorn.startGame(
      StartGame(
        creatorWelcome.gameId,creatorWelcome.playerId, creatorWelcome.playerKey,
        playerOrder = playerOrder.map(_.playerId)
      ), context(creatorAddress)
    )
  }

  def placeDisc(disc: Disc, welcome: Welcome, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    Thorn.placeDisc(
      PlaceDisc(
        welcome.gameId, welcome.playerId, welcome.playerKey, disc
      ), context
    )
  }

  def bid(count: Int, welcome: Welcome, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    Thorn.bid(
      Bid(
        welcome.gameId, welcome.playerId, welcome.playerKey, count
      ), context
    )
  }
}
