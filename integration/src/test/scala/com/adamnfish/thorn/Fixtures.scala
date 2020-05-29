package com.adamnfish.thorn

import com.adamnfish.thorn.attempt.Attempt
import com.adamnfish.thorn.logic.Games
import com.adamnfish.thorn.Thorn
import com.adamnfish.thorn.models.{Bid, Context, CreateGame, Disc, Flip, GameStatus, JoinGame, Pass, Ping, PlaceDisc, PlayerAddress, PlayerId, Reconnect, Response, StartGame, Welcome}

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

  def pass(welcome: Welcome, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    Thorn.pass(
      Pass(
        welcome.gameId, welcome.playerId, welcome.playerKey
      ), context
    )
  }

  def flip(stackId: PlayerId, welcome: Welcome, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    Thorn.flip(
      Flip(
        welcome.gameId, welcome.playerId, welcome.playerKey, stackId
      ), context
    )
  }

  def ping(welcome: Welcome, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    Thorn.ping(
      Ping(
        welcome.gameId, welcome.playerId, welcome.playerKey
      ),
      context
    )
  }

  def reconnect(welcome: Welcome, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    Thorn.reconnect(
      Reconnect(
        welcome.gameId, welcome.playerId, welcome.playerKey
      ),
      context
    )
  }
}
