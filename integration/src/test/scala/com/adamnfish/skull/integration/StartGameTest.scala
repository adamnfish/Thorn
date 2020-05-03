package com.adamnfish.skull.integration

import com.adamnfish.skull.Skull._
import com.adamnfish.skull.logic.Games
import com.adamnfish.skull.models.{CreateGame, JoinGame, PlayerAddress, PlayerKey, StartGame}
import com.adamnfish.skull.{AttemptValues, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{OneInstancePerTest, OptionValues}


class StartGameTest extends AnyFreeSpec with AttemptValues with OptionValues
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

        val player2Address = PlayerAddress("player-2-address")
        joinGame(
          JoinGame(code, "screen name 2"),
          context(player2Address)
        ).value()

        val startGameRequest = StartGame(creatorWelcome.gameId, creatorWelcome.playerId, creatorWelcome.playerKey)
        startGame(
          startGameRequest,
          context(creatorAddress)
        ).isSuccessfulAttempt()
      }
    }

    "sends a game summary to every player" in {
      withTestContext { (context, _) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val player2Address = PlayerAddress("player-address-2")
        val joinGameWelcome = joinGame(
          JoinGame(code, "screen name 2"),
          context(player2Address)
        ).value().response.value

        val startGameRequest = StartGame(creatorWelcome.gameId, creatorWelcome.playerId, creatorWelcome.playerKey)
        val response = startGame(
          startGameRequest,
          context(creatorAddress)
        ).value()

        response.messages.values.map(_.self.playerId).toSet shouldEqual Set(
          creatorWelcome.playerId,
          joinGameWelcome.playerId,
        )
      }
    }

    "doesn't return a response message" in {
      withTestContext { (context, _) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val player2Address = PlayerAddress("player-address-2")
        joinGame(
          JoinGame(code, "screen name 2"),
          context(player2Address)
        ).value()

        val startGameRequest = StartGame(creatorWelcome.gameId, creatorWelcome.playerId, creatorWelcome.playerKey)
        val response = startGame(
          startGameRequest,
          context(creatorAddress)
        ).value()
        response.response shouldEqual None
      }
    }

    "persists the game updates to the database" in {
      withTestContext { (context, db) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val player2Address = PlayerAddress("player-address-2")
        val joinWelcome = joinGame(
          JoinGame(code, "screen name 2"),
          context(player2Address)
        ).value().response.value

        val startGameRequest = StartGame(creatorWelcome.gameId, creatorWelcome.playerId, creatorWelcome.playerKey)
        val response = startGame(
          startGameRequest,
          context(creatorAddress)
        ).value()
        response.response shouldEqual None

        val gameDb = db.getGame(creatorWelcome.gameId).value().value
        gameDb.started shouldEqual true
        gameDb.playerIds.toSet shouldEqual Set(creatorWelcome.playerId.pid, joinWelcome.playerId.pid)
      }
    }
  }

  "for invalid cases" - {
    "fails if the player key does not match" in {
      withTestContext { (context, _) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val player2Address = PlayerAddress("player-2-address")
        joinGame(
          JoinGame(code, "screen name 2"),
          context(player2Address)
        ).value()

        val startGameRequest = StartGame(
          creatorWelcome.gameId, creatorWelcome.playerId,
          PlayerKey("INCORRECT KEY")
        )
        startGame(
          startGameRequest,
          context(creatorAddress)
        ).isFailedAttempt()
      }
    }

    "more cases" ignore {}
    // TODO:
  }
}
