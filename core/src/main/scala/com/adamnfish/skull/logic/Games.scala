package com.adamnfish.skull.logic

import java.time.ZonedDateTime

import com.adamnfish.skull.models.{Game, GameId, Player}
import java.util.UUID.randomUUID

import com.adamnfish.skull.attempt.{Attempt, Failure}


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

  def ensureNotStarted(game: Game): Attempt[Unit] = {
    if (game.started)
      Attempt.Left {
        Failure(
          "game has already started",
          "The game has already started",
          409
        ).asAttempt
      }
    else
      Attempt.unit
  }
}
