package com.adamnfish.thorn.integration

import com.adamnfish.thorn.{AttemptValues, Fixtures, ThornIntegration}
import com.adamnfish.thorn.models._
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec


class PingTest extends AnyFreeSpec with AttemptValues with OptionValues with ThornIntegration {

  "for a valid request" - {
    "is successful" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).isSuccessfulAttempt()

        Fixtures.ping(creatorWelcome, context(PlayerAddress("new address"))).isSuccessfulAttempt()
      }
    }

    "returns a game status to the player" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).isSuccessfulAttempt()

        val gameStatus = Fixtures.ping(creatorWelcome, context(PlayerAddress("new address"))).value().response.value
        gameStatus.self.playerId shouldEqual creatorWelcome.playerId
      }
    }

    "doesn't send any other messages" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).isSuccessfulAttempt()

        val messages = Fixtures.ping(creatorWelcome, context(PlayerAddress("new address"))).value().messages
        messages shouldBe empty
      }
    }

    "persists the change to the database" in {
      withTestContext { (context, db) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).isSuccessfulAttempt()
        Fixtures.ping(creatorWelcome, context(PlayerAddress("new address"))).isSuccessfulAttempt()

        val players = db.getPlayers(creatorWelcome.gameId).value()
        val updatedCreator = players.find(_.playerId == creatorWelcome.playerId.pid).value
        updatedCreator.playerAddress shouldEqual "new address"
      }
    }

    "fails if an incorrect player key is provided" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).isSuccessfulAttempt()

        Fixtures.ping(
          creatorWelcome.copy(playerKey = PlayerKey("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")),
          context(PlayerAddress("new address"))
        ).isFailedAttempt()
      }
    }
  }
}
