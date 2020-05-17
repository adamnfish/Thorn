package com.adamnfish.thorn.logic

import java.time.ZonedDateTime
import java.util.UUID.randomUUID

import com.adamnfish.thorn.attempt.{Attempt, Failure}
import com.adamnfish.thorn.models._


object Games {
  def newGame(gameName: String, creator: Player): Game = {
    val id = randomUUID().toString
    Game(
      gameId = GameId(id),
      gameName = gameName,
      creatorId = creator.playerId,
      players = List(creator),
      round = None,
      started = false,
      startTime = ZonedDateTime.now()
    )
  }

  def gameCode(gameId: GameId): String = {
    gameId.gid.take(4)
  }

  def addOrderedPlayerIds(gameDB: GameDB, orderedPlayerIds: List[PlayerId]): GameDB = {
    gameDB.copy(
      playerIds = orderedPlayerIds.map(_.pid)
    )
  }

  def addPlayerIds(gameDB: GameDB, playerDbs: List[PlayerDB]): GameDB = {
    gameDB.copy(
      playerIds = playerDbs.map(_.playerId)
    )
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

  def ensureCreator(playerId: PlayerId, game: Game): Attempt[Unit] = {
    if (game.creatorId == playerId)
      Attempt.unit
    else
      Attempt.Left {
        Failure(
          "Player is not the game creator", "You are not the game's creator",
          400
        )
      }
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

  def startGame(game: Game): Game = {
    game.copy(
      started = true,
      round = Some(InitialDiscs(
        firstPlayer = game.players.head.playerId,
        Map.empty
      ))
    )
  }

  def ensureNotAlreadyPlaying(game: Game, playerAddress: PlayerAddress): Attempt[Unit] = {
    if (game.players.exists(_.playerAddress == playerAddress))
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

  def ensureNoDuplicateScreenName(game: Game, screenName: String): Attempt[Unit] = {
    if (game.players.exists(_.screenName == screenName))
      Attempt.Left {
        Failure(
          "Duplicate screen name, joining game failed",
          "Someone else already has the same name!",
          400
        )
      }
    else
      Attempt.unit
  }

  def ensurePlayerKey(game: Game, playerId: PlayerId, playerKey: PlayerKey): Attempt[Unit] = {
    game.players.find(_.playerId == playerId).fold[Attempt[Unit]] {
      Attempt.Left(
        Failure(
          "Couldn't validate key for player that doesn not exist",
          "Couldn't find you in the game",
          404
        ).asAttempt
      )
    } { player =>
      if (player.playerKey == playerKey)
        Attempt.unit
      else
        Attempt.Left {
          Failure(
            "Invalid player key",
            "Couldn't authenticate you for this game",
            403
          )
        }
    }
  }

  def getPlayer(playerId: PlayerId, game: Game): Attempt[Player] = {
    Attempt.fromOption(
      game.players.find(_.playerId == playerId),
      Failure(
        s"Couldn't get player from game `${playerId}`:`${game.gameId.gid}`",
        "Couldn't find your player entry in the game",
        404
      ).asAttempt
    )
  }
}
