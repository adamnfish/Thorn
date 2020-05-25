package com.adamnfish.thorn.integration

import com.adamnfish.thorn.Thorn._
import com.adamnfish.thorn.logic.Games
import com.adamnfish.thorn.models._
import com.adamnfish.thorn.{AttemptValues, TestHelpers}
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec


class StartGameTest extends AnyFreeSpec with AttemptValues with OptionValues
  with ThornIntegration with TestHelpers {
  "for a valid request" - {
    "is successful" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value

        Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).isSuccessfulAttempt()
      }
    }

    "sends a game summary to every player" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        val response = Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).value()

        response.messages.values.map(_.self.playerId).toSet shouldEqual Set(
          creatorWelcome.playerId,
          joinGameWelcome.playerId,
        )
      }
    }

    "doesn't return a response message" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        val response = Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).value()

        response.response shouldEqual None
      }
    }

    "persists the game updates to the database" in {
      withTestContext { (context, db) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).value()
        val gameDb = db.getGame(creatorWelcome.gameId).value().value

        gameDb.started shouldEqual true
        gameDb.playerIds.toSet shouldEqual Set(
          creatorWelcome.playerId.pid,
          joinGameWelcome.playerId.pid
        )
      }
    }
  }

  "for invalid cases" - {
    val createGameRequest = CreateGame("creator name", "game name")
    val creatorAddress = PlayerAddress("creator-address")

    "fails if the player key does not match" in {
      withTestContext { (context, _) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val player2Address = PlayerAddress("player-2-address")
        val joinGameWelcome = joinGame(
          JoinGame(code, "screen name 2"),
          context(player2Address)
        ).value().response.value

        val startGameRequest = StartGame(
          creatorWelcome.gameId, creatorWelcome.playerId,
          PlayerKey("INCORRECT KEY"),
          List(creatorWelcome.playerId, joinGameWelcome.playerId)
        )
        startGame(
          startGameRequest,
          context(creatorAddress)
        ).isFailedAttempt()
      }
    }

    "fails if this is not the creator" in {
      withTestContext { (context, _) =>
        val creatorWelcome = createGame(
          createGameRequest,
          context(creatorAddress)
        ).value().response.value
        val code = Games.gameCode(creatorWelcome.gameId)

        val player2Address = PlayerAddress("player-2-address")
        val joinGameWelcome = joinGame(
          JoinGame(code, "screen name 2"),
          context(player2Address)
        ).value().response.value

        val startGameRequest = StartGame(
          creatorWelcome.gameId, creatorWelcome.playerId,
          joinGameWelcome.playerKey,
          List(creatorWelcome.playerId, joinGameWelcome.playerId)
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
