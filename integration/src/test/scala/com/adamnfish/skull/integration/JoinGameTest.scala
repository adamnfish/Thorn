package com.adamnfish.skull.integration

import com.adamnfish.skull.Skull.{createGame, joinGame}
import com.adamnfish.skull.logic.Games
import com.adamnfish.skull.models.{CreateGame, JoinGame, PlayerAddress}
import com.adamnfish.skull.{AttemptValues, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{OneInstancePerTest, OptionValues}


class JoinGameTest extends AnyFreeSpec with AttemptValues with OptionValues
  with SkullIntegration with OneInstancePerTest with TestHelpers {

  val createGameRequest = CreateGame("creator name", "game name")
  val creatorAddress = PlayerAddress("creator-address")

  "for a valid request" - {
    "is successful" in {
      withTestContext { (context, _) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val player2Address = PlayerAddress("player-address-2")
        joinGame(
          JoinGame(code, "player screen name"),
          context(player2Address)
        ).isSuccessfulAttempt()
      }
    }

    "allows a second player to join" in {
      withTestContext { (context, _) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val player2Address = PlayerAddress("player-address-2")
        joinGame(
          JoinGame(code, "player screen name"),
          context(player2Address)
        ).isSuccessfulAttempt()

        val player3Address = PlayerAddress("player-address-3")
        joinGame(
          JoinGame(code, "player screen name 2"),
          context(player3Address)
        ).isSuccessfulAttempt()
      }
    }

    "doesn't send any other messages out" in {
      withTestContext { (context, _) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val player2Address = PlayerAddress("player-address-2")
        joinGame(
          JoinGame(code, "player screen name"),
          context(player2Address)
        ).value().messages shouldBe empty
      }
    }

    "returns a correct welcome message" in {
      withTestContext { (context, _) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val player2Address = PlayerAddress("player-address-2")
        joinGame(
          JoinGame(code, "player screen name"),
          context(player2Address)
        ).value().response.nonEmpty shouldEqual true
      }
    }

    "persists the new player to the database" in {
      withTestContext { (context, db) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val player2Address = PlayerAddress("player-address-2")
        val welcomeMessage = joinGame(
          JoinGame(code, "player screen name"),
          context(player2Address)
        ).value().response.value

        val playerDb = db.getPlayers(welcomeMessage.gameId).value()
          .find(_.playerId == welcomeMessage.playerId.pid).value
        playerDb should have(
          "playerKey" as welcomeMessage.playerKey.key,
          "playerId" as welcomeMessage.playerId.pid,
        )
      }
    }
  }

  "for invalid cases" - {
    "fails if the player address is already in use" in {
      withTestContext { (context, _) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        joinGame(
          JoinGame(code, "player screen name"),
          context(creatorAddress)  // re-use existing address
        ).isFailedAttempt()
      }
    }

    "fails if the player's screen name is already in use" in {
      withTestContext { (context, _) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val player2Address = PlayerAddress("player-address-2")
        joinGame(
          JoinGame(code, createGameRequest.screenName),// re-use existing screen name
          context(player2Address)
        ).isFailedAttempt()
      }
    }

    "more cases" ignore {}
    // TODO:
  }
}
