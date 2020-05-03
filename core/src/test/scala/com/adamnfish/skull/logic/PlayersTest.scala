package com.adamnfish.skull.logic

import java.time.ZonedDateTime

import org.scalatest.freespec.AnyFreeSpec
import Players._
import com.adamnfish.skull.AttemptValues
import com.adamnfish.skull.models.{GameDB, PlayerAddress, PlayerDB}
import org.scalatest.matchers.should.Matchers


class PlayersTest extends AnyFreeSpec with Matchers with AttemptValues {
  "newPlayer" - {
    val screenName = "screen-name"
    val address = PlayerAddress("address")

    "uses provided screen name" in {
      newPlayer(screenName, address).screenName shouldEqual screenName
    }

    "uses provided address" in {
      newPlayer(screenName, address).playerAddress shouldEqual address
    }

    "sets score to 0" in {
      newPlayer(screenName, address).score shouldEqual 0
    }

    "sets a random ID" in {
      val id1 = newPlayer(screenName, address).playerId
      val id2 = newPlayer(screenName, address).playerId
      id1 should not equal id2
    }

    "sets a random key" in {
      val key1 = newPlayer(screenName, address).playerKey
      val key2 = newPlayer(screenName, address).playerKey
      key1 should not equal key2
    }
  }

  "ensureAllPlayersPresent" - {
    val player1 = PlayerDB(
      "gid", "pid1", "pk", "pa", "sn", 0, Nil, None, None
    )
    val player2 = PlayerDB(
      "gid", "pid2", "pk", "pa", "sn", 0, Nil, None, None
    )
    val gameDB = GameDB(
      "gcode", "gid", "name",
      playerIds = List(),
      false, ZonedDateTime.now(), "none", None, Map.empty
    )

    "succeeds if all players are present" in {
      ensureAllPlayersPresent(
        gameDB.copy(playerIds = List(player1.playerId, player2.playerId)),
        List(player1, player2)
      ).isSuccessfulAttempt()
    }

    "fails if a player is missing" in {
      ensureAllPlayersPresent(
        gameDB.copy(playerIds = List(player1.playerId, player2.playerId)),
        List(player1)
      ).isFailedAttempt()
    }
  }

  "startPlayer" - {
    val player1 = newPlayer("player 1", PlayerAddress("a1"))
    val player2 = newPlayer("player 2", PlayerAddress("a2"))

    "returns one of the provided players" in {
      val players = Map(
        player1.playerId -> player1,
        player2.playerId -> player2
      )
      val result = startPlayer(players).value()
      result should (
        equal (player1.playerId) or
        equal (player2.playerId)
      )
    }

    "fails if players is empty" in {
      startPlayer(Map.empty).isFailedAttempt()
    }
  }
}
