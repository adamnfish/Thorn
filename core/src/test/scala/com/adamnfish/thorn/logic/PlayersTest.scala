package com.adamnfish.thorn.logic

import java.time.ZonedDateTime

import org.scalatest.freespec.AnyFreeSpec
import Players._
import com.adamnfish.thorn.{AttemptValues, TestHelpers}
import com.adamnfish.thorn.models.{GameDB, PlayerAddress, PlayerDB, PlayerId}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers


class PlayersTest extends AnyFreeSpec with Matchers with OptionValues with AttemptValues with TestHelpers {
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

  "removeDiscFromThisPlayer" - {
    val player1 = newPlayer("test1", PlayerAddress("address1"))
    val player2 = newPlayer("test2", PlayerAddress("address2"))

    "returns players list where the player has one fewer disc" in {
      val pid = player1.playerId
      val players = removeDiscFromThisPlayer(pid, List(player1, player2)).value()
      val updatedPlayer = players.find(_.playerId == pid).value
      discCount(updatedPlayer) shouldEqual (discCount(player1) - 1)
    }

    "fails if the player does not exist" in {
      val result = removeDiscFromThisPlayer(PlayerId("does not exist"), List(player1, player2))
      result.isFailedAttempt()
    }
  }

  "removePlayerDisc" - {
    val player = newPlayer("test", PlayerAddress("address"))
    sealed trait DiscRemoved
    object RoseRemoved extends DiscRemoved
    object ThornRemoved extends DiscRemoved

    "reduces the disc count" - {
      "for a player with all discs" in {
        discCount(removePlayerDisc(
          player.copy(
            roseCount = 3,
            hasThorn = true,
          )
        )) shouldEqual 3
      }

      "for a player with some discs" in {
        discCount(removePlayerDisc(
          player.copy(
            roseCount = 2,
            hasThorn = true,
          )
        )) shouldEqual 2
      }

      "for a player with just one Rose" in {
        discCount(removePlayerDisc(
          player.copy(
            roseCount = 1,
            hasThorn = false,
          )
        )) shouldEqual 0
      }

      "for a player with just a Thorn" in {
        discCount(removePlayerDisc(
          player.copy(
            roseCount = 0,
            hasThorn = true,
          )
        )) shouldEqual 0
      }
    }

    "removes Roses more often than a Thorns when there are more of them" in {
      val result = runMultiple(100) { _ =>
        val testPlayer = player.copy(
          roseCount = 6,
          hasThorn = true,
        )
        val updatedPlayer = removePlayerDisc(testPlayer)
        if (!updatedPlayer.hasThorn)
          Right(ThornRemoved)
        else if (updatedPlayer.roseCount == testPlayer.roseCount - 1)
          Right(RoseRemoved)
        else if (updatedPlayer.roseCount == testPlayer.roseCount)
          Left(s"No disc removed - rose count: ${updatedPlayer.roseCount}, thorn: ${updatedPlayer.hasThorn}")
        else
          Left(s"Unexpected player state after disc removal - rose count: ${updatedPlayer.roseCount}, thorn: ${updatedPlayer.hasThorn}")
      }
      result.fold(
        { errs =>
          fail(errs.distinct.mkString(", "))
        },
        { results =>
          val roseRemovals = results.count(_ == RoseRemoved)
          val thornRemovals = results.count(_ == ThornRemoved)
          withClue("(This is a rough test that will fail occasionally)") {
            roseRemovals should be > (thornRemovals * 2)
          }
        }
      )
    }
  }

  "outOfDiscs" - {
    "false for new player" in {
      val player = newPlayer("name", PlayerAddress("address"))
      outOfDiscs(player) shouldEqual false
    }

    "false for a player with Roses but no Thorn" in {
      val player = newPlayer("name", PlayerAddress("address"))
      outOfDiscs(
        player.copy(
          hasThorn = false
        )
      ) shouldEqual false
    }

    "false for a player with few Roses and no Thorn" in {
      val player = newPlayer("name", PlayerAddress("address"))
      outOfDiscs(
        player.copy(
          hasThorn = false,
          roseCount = 1,
        )
      ) shouldEqual false
    }

    "false for a player with a Thorn and no Roses" in {
      val player = newPlayer("name", PlayerAddress("address"))
      outOfDiscs(
        player.copy(
          hasThorn = true,
          roseCount = 0,
        )
      ) shouldEqual false
    }

    "true for a player with no discs" in {
      val player = newPlayer("name", PlayerAddress("address"))
      outOfDiscs(
        player.copy(
          hasThorn = false,
          roseCount = 0,
        )
      ) shouldEqual true
    }
  }
}
