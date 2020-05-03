package com.adamnfish.skull.integration

import com.adamnfish.skull.Skull.createGame
import com.adamnfish.skull.models.{CreateGame, PlayerAddress}
import com.adamnfish.skull.{AttemptValues, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{OneInstancePerTest, OptionValues}


class CreateGameTest extends AnyFreeSpec with AttemptValues with OptionValues
  with SkullIntegration with OneInstancePerTest with TestHelpers {

  val validRequest = CreateGame("screen name", "game name")
  val creatorAddress = PlayerAddress("creator-address")

  "for a valid request" - {
    "is successful" in {
      withTestContext { (context, _) =>
        createGame(
          validRequest,
          context(creatorAddress)
        ).isSuccessfulAttempt()
      }
    }

    "doesn't send any other messages out" in {
      withTestContext { (context, _) =>
        val response = createGame(
          validRequest,
          context(creatorAddress)
        ).value()
        response.messages shouldBe empty
      }
    }

    "returns a correct welcome message" in {
      withTestContext { (context, _) =>
        val response = createGame(
          validRequest,
          context(creatorAddress)
        ).value()
        response.response.nonEmpty shouldEqual true
      }
    }

    "persists the saved game to the database" in {
      withTestContext { (context, db) =>
        val response = createGame(
          validRequest,
          context(creatorAddress)
        ).value()
        val welcomeMessage = response.response.value
        val gameDb = db.getGame(welcomeMessage.gameId).value().value
        gameDb.gameId shouldEqual welcomeMessage.gameId.gid
      }
    }

    "persists the saved creator to the database" in {
      withTestContext { (context, db) =>
        val response = createGame(
          validRequest,
          context(creatorAddress)
        ).value()
        val welcomeMessage = response.response.value
        val creatorDb = db.getPlayers(welcomeMessage.gameId).value().head
        creatorDb should have(
          "playerKey" as welcomeMessage.playerKey.key,
          "playerId" as welcomeMessage.playerId.pid,
        )
      }
    }
  }

  "for invalid cases" - {
    // TODO: more of these
    "more cases" ignore {}
  }
}
