package com.adamnfish.thorn.integration

import java.time.ZonedDateTime

import com.adamnfish.thorn.models.{CreateGame, PlayerAddress}
import com.adamnfish.thorn.{AttemptValues, Fixtures, TestHelpers, ThornIntegration}
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec


class CreateGameTest extends AnyFreeSpec with AttemptValues with OptionValues
  with ThornIntegration with TestHelpers {

  val validRequest = CreateGame("screen name", "game name")
  val creatorAddress = PlayerAddress("creator-address")

  "for a valid request" - {
    "is successful" in {
      withTestContext { (context, _) =>
        Fixtures.createGame(context).isSuccessfulAttempt()
      }
    }

    // TODO: improve this test to check the game details
    "sends a status message out to the creator" in {
      withTestContext { (context, _) =>
        val response = Fixtures.createGame(context).value()
        response.messages.size shouldEqual 1
      }
    }

    "returns a correct welcome message" in {
      withTestContext { (context, _) =>
        val response = Fixtures.createGame(context).value()
        response.response.nonEmpty shouldEqual true
      }
    }

    "persists the saved game to the database" - {
      "with key fields" in {
        withTestContext { (context, db) =>
          val response = Fixtures.createGame(context).value()
          val welcomeMessage = response.response.value
          val gameDb = db.getGame(welcomeMessage.gameId).value().value
          gameDb should have(
            "gameId" as welcomeMessage.gameId.gid,
            "creatorId" as welcomeMessage.playerId.pid,
          )
        }
      }

      "with an appropriate expiry" in {
        withTestContext { (context, db) =>
          val response = Fixtures.createGame(context).value()
          val welcomeMessage = response.response.value
          val gameDb = db.getGame(welcomeMessage.gameId).value().value

          val roughExpiry = ZonedDateTime.now().plusDays(21)
          val tooEarly = roughExpiry.minusDays(1).toEpochSecond
          val tooLate = roughExpiry.plusDays(1).toEpochSecond

          gameDb.expiry should (be > tooEarly and be < tooLate)
        }
      }
    }

    "persists the saved creator to the database" - {
      "with key fields" in {
        withTestContext { (context, db) =>
          val response = Fixtures.createGame(context).value()
          val welcomeMessage = response.response.value
          val creatorDb = db.getPlayers(welcomeMessage.gameId).value().head
          creatorDb should have(
            "playerKey" as welcomeMessage.playerKey.key,
            "playerId" as welcomeMessage.playerId.pid,
          )
        }
      }

      "an appropriate expiry" in {
        withTestContext { (context, db) =>
          val response = Fixtures.createGame(context).value()
          val welcomeMessage = response.response.value
          val creatorDb = db.getPlayers(welcomeMessage.gameId).value().head

          val roughExpiry = ZonedDateTime.now().plusDays(21)
          val tooEarly = roughExpiry.minusDays(1).toEpochSecond
          val tooLate = roughExpiry.plusDays(1).toEpochSecond

          creatorDb.expiry should (be > tooEarly and be < tooLate)
        }
      }
    }
  }

  "for invalid cases" - {
    // TODO: more of these
    "more cases" ignore {}
  }
}
