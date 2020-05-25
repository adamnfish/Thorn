package com.adamnfish.thorn.integration

import com.adamnfish.thorn.{AttemptValues, TestHelpers}
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec


class PingTest extends AnyFreeSpec with AttemptValues with OptionValues
  with ThornIntegration with TestHelpers {

  "for a newly created game" - {
    "is successful" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        Fixtures.ping(creatorWelcome, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
      }
    }

    "returns a correct response status message" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val message = Fixtures.ping(creatorWelcome, context(Fixtures.creatorAddress)).value().response.value
        message.self should have(
          "screenName" as "creator name",
          "playerId" as creatorWelcome.playerId.pid,
          "score" as 0,
          "placedDiscs" as None,
          "roseCount" as 3,
          "hasThorn" as true,
        )
        message.game should have(
          "gameId" as creatorWelcome.gameId.gid,
          "gameName" as "game name",
          "creatorId" as creatorWelcome.playerId.pid,
        )
        message.game.round shouldEqual None
      }
    }

    "does not send messages to anyone else" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val messages = Fixtures.ping(creatorWelcome, context(Fixtures.creatorAddress)).value().messages
        messages shouldBe empty
      }
    }
  }
}
