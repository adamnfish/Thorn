package com.adamnfish.skull.logic

import java.time.ZonedDateTime

import org.scalatest.freespec.AnyFreeSpec
import Games._
import com.adamnfish.skull.models.{Player, PlayerAddress, PlayerId, PlayerKey}
import org.scalatest.matchers.should.Matchers


class GamesTest extends AnyFreeSpec with Matchers {
  "newGame" - {
    val creator = Player(
      screenName = "screen-name",
      playerId = PlayerId("id"),
      playerKey = PlayerKey("key"),
      playerAddress = PlayerAddress("address"),
      0
    )

    "sets a random game id" in {
      val firstId = newGame(creator)
      val secondId = newGame(creator)
      firstId should not equal secondId
    }

    "sets started to false" in {
      newGame(creator).started shouldEqual false
    }

    "sets round to None" in {
      newGame(creator).round shouldEqual None
    }

    "adds creator to game players" in {
      newGame(creator).players shouldEqual List(creator)
    }

    "start time is close enough to 'now' for testing purposes" in {
      val now = ZonedDateTime.now()
      val startTime = newGame(creator).startTime
      val notTooEarly = startTime.isAfter(now.minusSeconds(10))
      val notTooLate = startTime.isBefore(now.plusSeconds(10))
      notTooEarly shouldEqual true
      notTooLate shouldEqual true
    }
  }
}
