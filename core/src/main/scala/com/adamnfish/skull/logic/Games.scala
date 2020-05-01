package com.adamnfish.skull.logic

import java.time.ZonedDateTime

import com.adamnfish.skull.models.{Game, GameId, Player}
import java.util.UUID.randomUUID


object Games {
  def newGame(gameName: String, creator: Player): Game = {
    val id = randomUUID().toString
    Game(
      gameId = GameId(id),
      gameName = gameName,
      players = Map(creator.playerId -> creator),
      round = None,
      started = false,
      startTime = ZonedDateTime.now()
    )
  }

  def gameCode(gameId: GameId): String = {
    gameId.gid.take(4)
  }
}
