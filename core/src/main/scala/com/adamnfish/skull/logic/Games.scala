package com.adamnfish.skull.logic

import java.time.ZonedDateTime
import java.util.UUID.randomUUID

import com.adamnfish.skull.attempt.{Attempt, Failure}
import com.adamnfish.skull.models._


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

  def requireGame[A](gameOpt: Option[A], query: String): Attempt[A] = {
    Attempt.fromOption(gameOpt,
      Failure(
        s"Game not found for lookup $query",
        "Couldn't find game. If it's old it may have been automatically deleted?",
        404
      ).asAttempt
    )
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

  def startGame(game: Game, startPlayer: PlayerId): Game = {
    game.copy(
      started = true,
      round = Some(InitialDiscs(
        firstPlayer = startPlayer,
        Map.empty
      ))
    )
  }

  def ensureNotAlreadyPlaying(game: Game, playerAddress: PlayerAddress): Attempt[Unit] = {
    if (game.players.values.exists(_.playerAddress == playerAddress))
      Attempt.Left {
        Failure(
          "Duplicate player address, joining game failed",
          "You can't join the same game twice",
          400
        )
      }
    else
      Attempt.unit
  }
}
