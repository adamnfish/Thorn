package com.adamnfish.skull.integration

import com.adamnfish.skull.Skull._
import com.adamnfish.skull.logic.Games
import com.adamnfish.skull.models.{CreateGame, JoinGame, PlayerAddress, StartGame}
import com.adamnfish.skull.{AttemptValues, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{OneInstancePerTest, OptionValues}


class StartGameTest extends AnyFreeSpec with AttemptValues with OptionValues
  with SkullIntegration with OneInstancePerTest with TestHelpers {
  "for a valid request" - {
    val createGameRequest = CreateGame("creator name", "game name")

    "is successful" in {
      withTestContext("player-address".address) { context =>
        val creatorWelcome = createGame(createGameRequest, context).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)
        asAnotherPlayer(context, PlayerAddress("player-address-2")) { player2Context =>
          joinGame(
            JoinGame(code, "screen name 2"),
            player2Context
          ).value()
        }
        val startGameRequest = StartGame(creatorWelcome.gameId, creatorWelcome.playerId, creatorWelcome.playerKey)
        startGame(startGameRequest, context).isSuccessfulAttempt()
      }
    }

    "sends a game summary to every player" in {
      withTestContext("player-address".address) { context =>
        val creatorWelcome = createGame(createGameRequest, context).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val joinGameWelcome =
          asAnotherPlayer(context, PlayerAddress("player-address-2")) { player2Context =>
            joinGame(
              JoinGame(code, "screen name 2"),
              player2Context
            ).value().response.value
          }

        val startGameRequest = StartGame(creatorWelcome.gameId, creatorWelcome.playerId, creatorWelcome.playerKey)
        val response = startGame(startGameRequest, context).value()
        response.messages.values.map(_.self.playerId).toSet shouldEqual Set(
          creatorWelcome.playerId,
          joinGameWelcome.playerId,
        )
      }
    }

    "doesn't return a response message" in {
      withTestContext("player-address".address) { context =>
        val creatorWelcome = createGame(createGameRequest, context).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        asAnotherPlayer(context, PlayerAddress("player-address-2")) { player2Context =>
          joinGame(
            JoinGame(code, "screen name 2"),
            player2Context
          ).value()
        }
        val startGameRequest = StartGame(creatorWelcome.gameId, creatorWelcome.playerId, creatorWelcome.playerKey)

        val response = startGame(startGameRequest, context).value()
        response.response shouldEqual None
      }
    }

    "persists the game updates to the database" in {
      withTestContext("player-address".address) { context =>
        val creatorWelcome = createGame(createGameRequest, context).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val joinWelcome =
          asAnotherPlayer(context, PlayerAddress("player-address-2")) { player2Context =>
            joinGame(
              JoinGame(code, "screen name 2"),
              player2Context
            ).value().response.value
          }

        val startGameRequest = StartGame(creatorWelcome.gameId, creatorWelcome.playerId, creatorWelcome.playerKey)
        val response = startGame(startGameRequest, context).value()
        response.response shouldEqual None

        val gameDb = context.db.getGame(creatorWelcome.gameId).value().value
        gameDb.started shouldEqual true
        gameDb.playerIds.toSet shouldEqual Set(creatorWelcome.playerId.pid, joinWelcome.playerId.pid)
      }
    }
  }

  "for invalid cases" ignore {
    // TODO:
  }
}
