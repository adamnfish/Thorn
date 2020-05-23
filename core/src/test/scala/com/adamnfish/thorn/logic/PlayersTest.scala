package com.adamnfish.thorn.logic

import java.time.ZonedDateTime

import org.scalatest.freespec.AnyFreeSpec
import Players._
import com.adamnfish.thorn.AttemptValues
import com.adamnfish.thorn.models.{GameDB, PlayerAddress, PlayerDB}
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
      "gid", "pid1", "pk", "pa", "sn", 0, Nil, 3, hasThorn = true, 0, false
    )
    val player2 = PlayerDB(
      "gid", "pid2", "pk", "pa", "sn", 0, Nil, 3, hasThorn = true, 0, false
    )
    val gameDB = GameDB(
      "gcode", "gid", "name",
      player1.playerId,
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
}
