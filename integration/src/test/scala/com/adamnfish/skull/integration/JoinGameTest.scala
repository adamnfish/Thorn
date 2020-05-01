package com.adamnfish.skull.integration

import com.adamnfish.skull.Skull.{createGame, joinGame}
import com.adamnfish.skull.logic.Games
import com.adamnfish.skull.models.{CreateGame, JoinGame, PlayerAddress}
import com.adamnfish.skull.{AttemptValues, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{OneInstancePerTest, OptionValues}


class JoinGameTest extends AnyFreeSpec with AttemptValues with OptionValues
  with SkullIntegration with OneInstancePerTest with TestHelpers {
  "for a valid request" - {
    val createGameRequest = CreateGame("screen name", "game name")

    "is successful" in {
      withTestContext("player-address".address) { context =>
        val creatorWelcome = createGame(createGameRequest, context).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)
        joinGame(
          JoinGame(code, "screen name"),
          context
        ).isSuccessfulAttempt() shouldEqual true
      }
    }

    "doesn't send any other messages out" in {
      withTestContext("player-address".address) { context =>
        val creatorWelcome = createGame(createGameRequest, context).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)
        joinGame(
          JoinGame(code, "screen name"),
          context
        ).value().messages shouldBe empty
      }
    }

    "returns a correct welcome message" in {
      withTestContext("player-address".address) { context =>
        val creatorWelcome = createGame(createGameRequest, context).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)
        joinGame(
          JoinGame(code, "screen name"),
          context
        ).value().response.nonEmpty shouldEqual true
      }
    }

    "persists the new player to the database" in {
      withTestContext("player-address".address) { context =>
        val creatorWelcome = createGame(createGameRequest, context).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)
        val welcomeMessage = joinGame(
          JoinGame(code, "screen name"),
          context
        ).value().response.value
        val playerDb = context.db.getPlayers(welcomeMessage.gameId).value()
          .find(_.playerId == welcomeMessage.playerId.pid).value
        playerDb should have(
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
