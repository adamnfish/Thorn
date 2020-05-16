package com.adamnfish.skull.validation

import java.util.UUID.randomUUID

import com.adamnfish.skull.models._
import com.adamnfish.skull.validation.Validation.validate
import com.adamnfish.skull.{AttemptValues, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class ValidationTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks
  with TestHelpers with AttemptValues {

  "validate createGame" - {
    val createGame = CreateGame("screen-name", "game-name")

    "accepts a valid createGame object" in {
      validate(createGame).isSuccessfulAttempt()
    }

    "rejected if screen name is empty" in {
      validate(
        createGame.copy(screenName = "")
      ).isFailedAttempt()
    }

    "rejected if game name is empty" in {
      validate(
        createGame.copy(gameName = "")
      ).isFailedAttempt()
    }

    "rejected if screen name is too long" in {
      validate(
        createGame.copy(screenName = "screen-name" * 10)
      ).isFailedAttempt()
    }

    "rejected if game name is too long" in {
      validate(
        createGame.copy(gameName = "game-name" * 10)
      ).isFailedAttempt()
    }

    "returns all failures if multiple conditions fail" in {
      val createGame = CreateGame("", "")
      validate(createGame).leftValue().failures.length shouldEqual 2
    }
  }

  "validate JoinGame" - {
    val joinGame = JoinGame("abcd", "screen name")

    "accepts a valid join game request" in {
      validate(joinGame).isSuccessfulAttempt()
    }

    "rejected if game code is empty" in {
      validate(
        joinGame.copy(
          gameCode = ""
        )
      ).isFailedAttempt()
    }

    "rejects an invalid game code" in {
      validate(
        joinGame.copy(
          gameCode = "not a game code"
        )
      ).isFailedAttempt()
    }

    "rejects a long screen name" in {
      validate(
        joinGame.copy(
          screenName = "screen-name" * 10
        )
      ).isFailedAttempt()
    }

    "rejects an empty screen name" in {
      validate(
        joinGame.copy(
          screenName = ""
        )
      ).isFailedAttempt()
    }

    "returns all failures if multiple conditions fail" in {
      val joinGame = JoinGame("", "")
      validate(joinGame).leftValue().failures.length shouldEqual 2
    }
  }

  "validate StartGame" - {
    val creatorId = randomUUID().toString
    val player1Id = randomUUID().toString
    val player2Id = randomUUID().toString

    val startGame = StartGame(
      GameId(randomUUID().toString),
      PlayerId(creatorId),
      PlayerKey(randomUUID().toString),
      playerOrder = List(
        PlayerId(creatorId),
        PlayerId(player1Id),
        PlayerId(player2Id),
      )
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

    "rejects an empty player order" in {
      validate(
        startGame.copy(playerOrder = Nil)
      ).isFailedAttempt()
    }

    "rejects player order containing an invalid ID" in {
      validate(
        startGame.copy(playerOrder = List(PlayerId("abc")))
      ).isFailedAttempt()
    }

    "rejects player order containing multiple invalid IDs" in {
      validate(
        startGame.copy(
          playerOrder = List(
            PlayerId("bad ID"),
            PlayerId(player1Id),
            PlayerId("another bad ID"),
          )
        )
      ).isFailedAttempt()
    }

    "returns all failures if multiple conditions fail" in {
      validate(
        StartGame(
          gameId = GameId("not a UUID"),
          playerId = PlayerId("not a UUID"),
          playerKey = PlayerKey("not a UUID"),
          playerOrder = Nil
        )
      ).leftValue().failures.length shouldEqual 4
    }
  }

  "validate PlaceDisc" - {
    val placeDisc = PlaceDisc(
      gameId = GameId(randomUUID().toString),
      playerId = PlayerId(randomUUID().toString),
      playerKey = PlayerKey(randomUUID().toString),
      disc = Rose
    )

    "accepts a valid Skull placement request" in {
      forAll { (disc: Disc) =>
        validate(
          placeDisc.copy(
            disc = disc
          )
        ).isSuccessfulAttempt()
      }
    }

    "rejects a request with an invalid game id" in {
      validate(
        placeDisc.copy(
          gameId = GameId("not a UUID"),
        )
      ).isFailedAttempt()
    }

    "rejects a request with an invalid player id" in {
      validate(
        placeDisc.copy(
          playerId = PlayerId("not a UUID"),
        )
      ).isFailedAttempt()
    }

    "rejects a request with an invalid player key" in {
      validate(
        placeDisc.copy(
          playerKey = PlayerKey("not a UUID"),
        )
      ).isFailedAttempt()
    }

    "returns all failures if multiple conditions fail" in {
      validate(
        PlaceDisc(
          gameId = GameId("not a UUID"),
          playerId = PlayerId("not a UUID"),
          playerKey = PlayerKey("not a UUID"),
          disc = Rose
        )
      ).leftValue().failures.length shouldEqual 3
    }
  }

  "validate Bid" - {
    val bid = Bid(
      gameId = GameId(randomUUID().toString),
      playerId = PlayerId(randomUUID().toString),
      playerKey = PlayerKey(randomUUID().toString),
      count = 1
    )

    "accepts a valid bid request" in {
      validate(bid).isSuccessfulAttempt()
    }

    "rejects a request with an invalid game id" in {
      validate(
        bid.copy(
          gameId = GameId("not a UUID")
        )
      ).isFailedAttempt()
    }

    "rejects a request with an invalid player id" in {
      validate(
        bid.copy(
          playerId = PlayerId("not a UUID"),
        )
      ).isFailedAttempt()
    }

    "rejects a request with an invalid player key" in {
      validate(
        bid.copy(
          playerKey = PlayerKey("not a UUID"),
        )
      ).isFailedAttempt()
    }

    "rejects a request with a negative bid count" in {
      validate(
        bid.copy(
          count = -1
        )
      ).isFailedAttempt()
    }

    "rejects a request with a 0 bid count" in {
      validate(
        bid.copy(
          count = 0
        )
      ).isFailedAttempt()
    }

    "returns all failures if multiple conditions fail" in {
      validate(
        Bid(
          gameId = GameId("not a UUID"),
          playerId = PlayerId("not a UUID"),
          playerKey = PlayerKey("not a UUID"),
          count = -1
        )
      ).leftValue().failures.length shouldEqual 4
    }
  }

  "validate Pass" - {
    val startGame = Pass(
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
        Pass(
          gameId = GameId("not a UUID"),
          playerId = PlayerId("not a UUID"),
          playerKey = PlayerKey("not a UUID"),
        )
      ).leftValue().failures.length shouldEqual 3
    }
  }

  "validate Flip" - {
    val flip = Flip(
      GameId(randomUUID().toString),
      PlayerId(randomUUID().toString),
      PlayerKey(randomUUID().toString),
      PlayerId(randomUUID().toString),
    )

    "accepts a valid start game request" in {
      validate(flip).isSuccessfulAttempt()
    }

    "rejects a non-UUID game ID" in {
      validate(
        flip.copy(gameId = GameId("not a UUID"))
      ).isFailedAttempt()
    }

    "rejects a non-UUID player ID" in {
      validate(
        flip.copy(playerId = PlayerId("not a UUID"))
      ).isFailedAttempt()
    }

    "rejects a non-UUID player key" in {
      validate(
        flip.copy(playerKey = PlayerKey("not a UUID"))
      ).isFailedAttempt()
    }

    "rejects a non-UUID stack" in {
      validate(
        flip.copy(stack = PlayerId("not a UUID"))
      ).isFailedAttempt()
    }

    "returns all failures if multiple conditions fail" in {
      validate(
        Flip(
          gameId = GameId("not a UUID"),
          playerId = PlayerId("not a UUID"),
          playerKey = PlayerKey("not a UUID"),
          stack = PlayerId("not a UUID"),
        )
      ).leftValue().failures.length shouldEqual 4
    }
  }

  "validate NewRound" - {
    val newRound = NewRound(
      GameId(randomUUID().toString),
      PlayerId(randomUUID().toString),
      PlayerKey(randomUUID().toString),
    )

    "accepts a valid start game request" in {
      validate(newRound).isSuccessfulAttempt()
    }

    "rejects a non-UUID game ID" in {
      validate(
        newRound.copy(gameId = GameId("not a UUID"))
      ).isFailedAttempt()
    }

    "rejects a non-UUID player ID" in {
      validate(
        newRound.copy(playerId = PlayerId("not a UUID"))
      ).isFailedAttempt()
    }

    "rejects a non-UUID player key" in {
      validate(
        newRound.copy(playerKey = PlayerKey("not a UUID"))
      ).isFailedAttempt()
    }

    "returns all failures if multiple conditions fail" in {
      validate(
        NewRound(
          GameId("not a UUID"),
          PlayerId("not a UUID"),
          PlayerKey("not a UUID"),
        )
      ).leftValue().failures.length shouldEqual 3
    }
  }

  "validate Reconnect" - {
    val reconnect = Reconnect(
      GameId(randomUUID().toString),
      PlayerId(randomUUID().toString),
      PlayerKey(randomUUID().toString),
    )

    "accepts a valid start game request" in {
      validate(reconnect).isSuccessfulAttempt()
    }

    "rejects a non-UUID game ID" in {
      validate(
        reconnect.copy(gameId = GameId("not a UUID"))
      ).isFailedAttempt()
    }

    "rejects a non-UUID player ID" in {
      validate(
        reconnect.copy(playerId = PlayerId("not a UUID"))
      ).isFailedAttempt()
    }

    "rejects a non-UUID player key" in {
      validate(
        reconnect.copy(playerKey = PlayerKey("not a UUID"))
      ).isFailedAttempt()
    }

    "returns all failures if multiple conditions fail" in {
      validate(
        Reconnect(
          GameId("not a UUID"),
          PlayerId("not a UUID"),
          PlayerKey("not a UUID"),
        )
      ).leftValue().failures.length shouldEqual 3
    }
  }

  "validate Ping" - {
    val ping = Ping(
      GameId(randomUUID().toString),
      PlayerId(randomUUID().toString),
      PlayerKey(randomUUID().toString),
    )

    "accepts a valid start game request" in {
      validate(ping).isSuccessfulAttempt()
    }

    "rejects a non-UUID game ID" in {
      validate(
        ping.copy(gameId = GameId("not a UUID"))
      ).isFailedAttempt()
    }

    "rejects a non-UUID player ID" in {
      validate(
        ping.copy(playerId = PlayerId("not a UUID"))
      ).isFailedAttempt()
    }

    "rejects a non-UUID player key" in {
      validate(
        ping.copy(playerKey = PlayerKey("not a UUID"))
      ).isFailedAttempt()
    }

    "returns all failures if multiple conditions fail" in {
      validate(
        Ping(
          GameId("not a UUID"),
          PlayerId("not a UUID"),
          PlayerKey("not a UUID"),
        )
      ).leftValue().failures.length shouldEqual 3
    }
  }
}
