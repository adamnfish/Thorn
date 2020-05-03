package com.adamnfish.skull.validation

import java.util.UUID.randomUUID

import com.adamnfish.skull.AttemptValues
import com.adamnfish.skull.models._
import com.adamnfish.skull.validation.Validation.validate
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class ValidationTest extends AnyFreeSpec with Matchers with AttemptValues {
  "validate createGame" - {
    "accepts a valid createGame object" in {
      val createGame = CreateGame("screen-name", "game-name")
      validate(createGame).isSuccessfulAttempt()
    }

    "rejected if screen name is empty" in {
      val createGame = CreateGame("", "game-name")
      validate(createGame).isFailedAttempt()
    }

    "rejected if game name is empty" in {
      val createGame = CreateGame("screen-name", "")
      validate(createGame).isFailedAttempt()
    }

    "rejected if screen name is too long" in {
      val createGame = CreateGame("screen-name" * 10, "game-name")
      validate(createGame).isFailedAttempt()
    }

    "rejected if game name is too long" in {
      val createGame = CreateGame("screen-name", "game-name" * 10)
      validate(createGame).isFailedAttempt()
    }

    "returns all failures if multiple conditions fail" in {
      val createGame = CreateGame("", "")
      validate(createGame).leftValue().failures.length shouldEqual 2
    }
  }

  "validate JoinGame" - {
    "accepts a valid join game request" in {
      val joinGame = JoinGame("abcd", "screen name")
      validate(joinGame).isSuccessfulAttempt()
    }

    "rejected if game code is empty" in {
      val joinGame = JoinGame("", "screen name")
      validate(joinGame).isFailedAttempt()
    }

    "rejects an invalid game code" in {
      val joinGame = JoinGame("not a game code", "screen name")
      validate(joinGame).isFailedAttempt()
    }

    "rejects a long screen name" in {
      val joinGame = JoinGame("not a game code", "screen-name" * 10)
      validate(joinGame).isFailedAttempt()
    }

    "rejects a short screen name" in {
      val joinGame = JoinGame("not a game code", "sn")
      validate(joinGame).isFailedAttempt()
    }

    "returns all failures if multiple conditions fail" in {
      val joinGame = JoinGame("", "")
      validate(joinGame).leftValue().failures.length shouldEqual 2
    }
  }

  "validate StartGame" - {
    val startGame = StartGame(
      GameId(randomUUID().toString),
      PlayerId(randomUUID().toString),
      PlayerKey(randomUUID().toString),
    )

    "accepts a valid start game request" in {
      validate(startGame).isSuccessfulAttempt()
    }

    "rejects a non-UUID game ID" in {
      validate(
        startGame.copy(gameId = GameId("not a UUID"))
      ).isFailedAttempt()
    }

    "rejects a non-UUID player ID" in {
      validate(
        startGame.copy(playerId = PlayerId("not a UUID"))
      ).isFailedAttempt()
    }

    "rejects a non-UUID player key" in {
      validate(
        startGame.copy(playerKey = PlayerKey("not a UUID"))
      ).isFailedAttempt()
    }

    "returns all failures if multiple conditions fail" in {
      validate(
        StartGame(
          gameId = GameId("not a UUID"),
          playerId = PlayerId("not a UUID"),
          playerKey = PlayerKey("not a UUID"),
        )
      ).leftValue().failures.length shouldEqual 3
    }
  }
}
