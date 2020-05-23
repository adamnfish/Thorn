package com.adamnfish.thorn.integration

import com.adamnfish.thorn.models._
import com.adamnfish.thorn.{AttemptValues, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{OneInstancePerTest, OptionValues}


class PassTest extends AnyFreeSpec with AttemptValues with OptionValues
  with ThornIntegration with OneInstancePerTest with TestHelpers with Journeys {

  "for valid requests" - {
    "when a player passes" - {
      "is successful" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()

          Fixtures.pass(testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()
        }
      }

      "sends a game status to every player" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          val messages = Fixtures.pass(testGame.player1, context(Fixtures.player1Address)).value().messages

          messages.keys.toSet shouldEqual Set(Fixtures.creatorAddress, Fixtures.player1Address, Fixtures.player2Address)
        }
      }

      "doesn't return a response message" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          val response = Fixtures.pass(testGame.player1, context(Fixtures.player1Address)).value()
          response.response shouldEqual None
        }
      }

      "includes player's pass status in the round summary" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          val messages = Fixtures.pass(testGame.player1, context(Fixtures.player1Address)).value().messages
          val round = messages.head._2.game.round.value
          round shouldBe a[BiddingSummary]
          val passed = round.asInstanceOf[BiddingSummary].passed
          passed should contain(testGame.player1.playerId)
        }
      }

      "persists player's bid to their database record" in {
        withTestContext { (context, db) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          Fixtures.pass(testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()

          val playerDbs = db.getPlayers(testGame.gameId).value()
          val creator = playerDbs.find(_.playerId == testGame.player1.playerId.pid).value
          creator.passed shouldEqual true
        }
      }

      "fails if it is not the player's turn" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.pass(testGame.player2, context(Fixtures.player2Address)).isFailedAttempt()
        }
      }
    }

    "when the last player passes" - {
      "is successful" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          Fixtures.pass(testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()
          Fixtures.pass(testGame.player2, context(Fixtures.player2Address)).isSuccessfulAttempt()
        }
      }

      "returns the new flipping round in the messages" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          Fixtures.pass(testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()
          val messages = Fixtures.pass(testGame.player2, context(Fixtures.player2Address)).value().messages

          val round = messages.head._2.game.round.value
          round shouldBe a[FlippingSummary]
          round.asInstanceOf[FlippingSummary] should have(
            "activePlayer" as testGame.creator.playerId.pid,
            "discs" as Map(
              testGame.creator.playerId -> 2,
              testGame.player1.playerId -> 2,
              testGame.player2.playerId -> 2,
            ),
            "revealed" as Map.empty,
          )
        }
      }

      "persists player's passed statuses to their database records" in {
        withTestContext { (context, db) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          Fixtures.pass(testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()
          Fixtures.pass(testGame.player2, context(Fixtures.player2Address)).isSuccessfulAttempt()
          val playerDbs = db.getPlayers(testGame.gameId).value()
          val bids = playerDbs.map(pdb => (pdb.playerId, pdb.passed))
          bids.toSet shouldEqual Set(
            testGame.creator.playerId.pid -> false,
            testGame.player1.playerId.pid -> true,
            testGame.player2.playerId.pid -> true,
          )
        }
      }

      "persists round change to game database" in {
        withTestContext { (context, db) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          Fixtures.pass(testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()
          Fixtures.pass(testGame.player2, context(Fixtures.player2Address)).isSuccessfulAttempt()

          val gameDb = db.getGame(testGame.gameId).value().value
          gameDb.roundState shouldEqual "flipping"
        }
      }
    }
  }
}
