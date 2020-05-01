package com.adamnfish.skull.logic

import java.time.ZonedDateTime

import org.scalatest.freespec.AnyFreeSpec
import Games._
import com.adamnfish.skull.AttemptValues
import com.adamnfish.skull.models.{Player, PlayerAddress, PlayerId, PlayerKey}
import org.scalatest.matchers.should.Matchers


class GamesTest extends AnyFreeSpec with Matchers with AttemptValues {
  "newGame" - {
    val gameName = "game-name"
    val creator = Player(
      screenName = "screen-name",
      playerId = PlayerId("id"),
      playerKey = PlayerKey("key"),
      playerAddress = PlayerAddress("address"),
      0
    )

    "sets a random game id" in {
      val firstId = newGame(gameName, creator)
      val secondId = newGame(gameName, creator)
      firstId should not equal secondId
    }

    "sets started to false" in {
      newGame(gameName, creator).started shouldEqual false
    }

    "sets round to None" in {
      newGame(gameName, creator).round shouldEqual None
    }

    "uses provided game name" in {
      newGame(gameName, creator).gameName shouldEqual gameName
    }

    "adds creator to game players" in {
      newGame(gameName, creator).players shouldEqual Map(creator.playerId -> creator)
    }

    "start time is close enough to 'now' for testing purposes" in {
      val now = ZonedDateTime.now()
      val startTime = newGame(gameName, creator).startTime
      val notTooEarly = startTime.isAfter(now.minusSeconds(10))
      val notTooLate = startTime.isBefore(now.plusSeconds(10))
      notTooEarly shouldEqual true
      notTooLate shouldEqual true
    }
  }

  "ensureNotStarted" - {
    val game = newGame(
      "test",
      Players.newPlayer("player", PlayerAddress("address"))
    )

    "success if the game has not started" in {
      game.copy(started = false)
      ensureNotStarted(
        game.copy(started = false)
      ).isSuccessfulAttempt() shouldEqual true
    }

    "failure if the game has already started" in {
      ensureNotStarted(
        game.copy(started = true)
      ).isFailedAttempt() shouldEqual true
    }
  }
}
