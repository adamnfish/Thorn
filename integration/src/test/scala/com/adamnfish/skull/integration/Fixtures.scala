package com.adamnfish.skull.integration

import com.adamnfish.skull.Skull
import com.adamnfish.skull.attempt.Attempt
import com.adamnfish.skull.logic.Games
import com.adamnfish.skull.models._
import javax.print.attribute.standard.MediaSize

import scala.concurrent.ExecutionContext


object Fixtures {
  val createGameRequest = CreateGame("creator name", "game name")
  val creatorAddress = PlayerAddress("creator-address")
  val player1Address = PlayerAddress("player-1-address")
  val player2Address = PlayerAddress("player-2-address")

  def createGame(context: PlayerAddress => Context)(implicit ec: ExecutionContext): Attempt[Response[Welcome]] = {
    Skull.createGame(
      createGameRequest,
      context(creatorAddress)
    )
  }

  def joinGame(creatorWelcome: Welcome, context: PlayerAddress => Context)(implicit ec: ExecutionContext): Attempt[Response[Welcome]] = {
    Skull.joinGame(
      JoinGame(
        Games.gameCode(creatorWelcome.gameId),
        "player 1"
      ), context(player1Address)
    )
  }

  def joinGame2(creatorWelcome: Welcome, context: PlayerAddress => Context)(implicit ec: ExecutionContext): Attempt[Response[Welcome]] = {
    Skull.joinGame(
      JoinGame(
        Games.gameCode(creatorWelcome.gameId),
        "player 2"
      ), context(player2Address)
    )
  }

  def startGame(creatorWelcome: Welcome, playerOrder: List[Welcome], context: PlayerAddress => Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    Skull.startGame(
      StartGame(
        creatorWelcome.gameId,creatorWelcome.playerId, creatorWelcome.playerKey,
        playerOrder = playerOrder.map(_.playerId)
      ), context(creatorAddress)
    )
  }

  def placeDisc(disc: Disc, welcome: Welcome, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    Skull.placeDisc(
      PlaceDisc(
        welcome.gameId, welcome.playerId, welcome.playerKey, disc
      ), context
    )
  }
}
