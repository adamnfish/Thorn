package com.adamnfish.skull.integration

import com.adamnfish.skull.{AttemptValues, TestHelpers}
import com.adamnfish.skull.Skull.createGame
import com.adamnfish.skull.models.CreateGame
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{OneInstancePerTest, OptionValues}


class CreateGameTest extends AnyFreeSpec with AttemptValues with OptionValues
  with SkullIntegration with OneInstancePerTest with TestHelpers {
  "for a valid request" - {
    val validRequest = CreateGame("screen name", "game name")

    "is successful" in {
      withTestContext("player-address".address) { context =>
        createGame(validRequest, context).isSuccessfulAttempt() shouldEqual true
      }
    }

    "doesn't send any other messages out" in {
      withTestContext("player-address".address) { context =>
        val response = createGame(validRequest, context).value()
        response.messages shouldBe empty
      }
    }

    "returns a correct welcome message" in {
      withTestContext("player-address".address) { context =>
        val response = createGame(validRequest, context).value()
        response.response.nonEmpty shouldEqual true
      }
    }

    "persists the saved game to the database" in {
      withTestContext("player-address".address) { context =>
        val response = createGame(validRequest, context).value()
        val welcomeMessage = response.response.value
        val gameDb = context.db.getGame(welcomeMessage.gameId).value().value
        gameDb.gameId shouldEqual welcomeMessage.gameId.gid
      }
    }

    "persists the saved creator to the database" in {
      withTestContext("player-address".address) { context =>
        val response = createGame(validRequest, context).value()
        val welcomeMessage = response.response.value
        val creatorDb = context.db.getPlayers(welcomeMessage.gameId).value().head
        creatorDb should have(
          "playerKey" as welcomeMessage.playerKey.key,
          "playerId" as welcomeMessage.playerId.pid,
        )
      }
    }
  }

  "for invalid cases" ignore {
    // TODO:
  }
}
