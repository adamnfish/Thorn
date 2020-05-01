package com.adamnfish.skull.validation

import com.adamnfish.skull.models.{CreateGame, JoinGame}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import Validation.validate
import com.adamnfish.skull.AttemptValues


class ValidationTest extends AnyFreeSpec with Matchers with AttemptValues {
  "validate createGame" - {
    "accepts a valid createGame object" in {
      val createGame = CreateGame("screen-name", "game-name")
      validate(createGame).isSuccessfulAttempt() shouldEqual true
    }

    "rejected if screen name is empty" in {
      val createGame = CreateGame("", "game-name")
      validate(createGame).isFailedAttempt() shouldEqual true
    }

    "rejected if game name is empty" in {
      val createGame = CreateGame("screen-name", "")
      validate(createGame).isFailedAttempt() shouldEqual true
    }

    "rejected if screen name is too long" in {
      val createGame = CreateGame("screen-name" * 10, "game-name")
      validate(createGame).isFailedAttempt() shouldEqual true
    }

    "rejected if game name is too long" in {
      val createGame = CreateGame("screen-name", "game-name" * 10)
      validate(createGame).isFailedAttempt() shouldEqual true
    }

    "returns all failures if multiple conditions fail" in {
      val createGame = CreateGame("", "")
      validate(createGame).leftValue().failures.length shouldEqual 2
    }
  }

  "validate JoinGame" - {
    "accepts a valid join game request" in {
      val joinGame = JoinGame("abcd", "screen name")
      validate(joinGame).isSuccessfulAttempt() shouldEqual true
    }

    "rejected if game code is empty" in {
      val joinGame = JoinGame("", "screen name")
      validate(joinGame).isFailedAttempt() shouldEqual true
    }

    "rejects an invalid game code" in {
      val joinGame = JoinGame("not a game code", "screen name")
      validate(joinGame).isFailedAttempt() shouldEqual true
    }

    "rejects a long screen name" in {
      val joinGame = JoinGame("not a game code", "screen-name" * 10)
      validate(joinGame).isFailedAttempt() shouldEqual true
    }

    "rejects a short screen name" in {
      val joinGame = JoinGame("not a game code", "sn")
      validate(joinGame).isFailedAttempt() shouldEqual true
    }

    "returns all failures if multiple conditions fail" in {
      val joinGame = JoinGame("", "")
      validate(joinGame).leftValue().failures.length shouldEqual 2
    }
  }
}
