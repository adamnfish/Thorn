package com.adamnfish.skull.integration

import com.adamnfish.skull.models._
import com.adamnfish.skull.{AttemptValues, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{OneInstancePerTest, OptionValues}


class PlaceDiscTest extends AnyFreeSpec with AttemptValues with OptionValues
  with SkullIntegration with OneInstancePerTest with TestHelpers {

  "for a valid request" - {
    "is successful" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        Fixtures.joinGame(creatorWelcome, context).value()
        Fixtures.startGame(creatorWelcome, context).isSuccessfulAttempt()

        Fixtures.placeDisc(
          Skull, creatorWelcome, context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
      }
    }

    "sends a game summary to every player" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        Fixtures.startGame(creatorWelcome, context).isSuccessfulAttempt()
        val response = Fixtures.placeDisc(
          Skull, creatorWelcome, context(Fixtures.creatorAddress)
        ).value()

        response.messages.values.map(_.self.playerId).toSet shouldEqual Set(
          creatorWelcome.playerId,
          joinGameWelcome.playerId,
        )
      }
    }

    "doesn't return a response message" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        Fixtures.joinGame(creatorWelcome, context).value().response.value
        Fixtures.startGame(creatorWelcome, context).value()
        val response = Fixtures.placeDisc(
          Skull, creatorWelcome, context(Fixtures.creatorAddress)
        ).value()

        response.response shouldEqual None
      }
    }

    "persists the game updates to the database" in {
      withTestContext { (context, db) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        Fixtures.joinGame(creatorWelcome, context).value()
        Fixtures.startGame(creatorWelcome, context).value()
        Fixtures.placeDisc(
          Skull, creatorWelcome, context(Fixtures.creatorAddress)
        ).value()
        val playerDbs = db.getPlayers(creatorWelcome.gameId).value()
        val creatorDb = playerDbs.find(_.playerId == creatorWelcome.playerId.pid).value

        creatorDb.discs shouldEqual List("skull")
      }
    }
  }

  "for invalid cases" - {
    "add cases" ignore {}
    // TODO:
  }
}
