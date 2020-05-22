package com.adamnfish.thorn.integration

import com.adamnfish.thorn.models._
import com.adamnfish.thorn.{AttemptValues, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{OneInstancePerTest, OptionValues}


class BidTest extends AnyFreeSpec with AttemptValues with OptionValues
  with ThornIntegration with OneInstancePerTest with TestHelpers with Journeys {

  "for valid requests" - {
    "opening the bidding from a place round" - {
      "successfully" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
        }
      }

      "sends a game status to every player" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          val messages = Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).value().messages
          messages.keys.toSet shouldEqual Set(Fixtures.creatorAddress, Fixtures.player1Address, Fixtures.player2Address)
        }
      }

      "doesn't return a response message" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          val response = Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).value()
          response.response shouldEqual None
        }
      }

      "round is advanced to bidding in the game status messages" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          val messages = Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).value().messages
          messages.head._2.game.round.value shouldBe a[BiddingSummary]
        }
      }

      "includes player's bid on the round summary" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          val messages = Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).value().messages
          val round = messages.head._2.game.round.value
          round shouldBe a[BiddingSummary]
          val playerBid = round.asInstanceOf[BiddingSummary].bids.get(testGame.creator.playerId).value
          playerBid shouldEqual 1
        }
      }

      "persists round status to game database" in {
        withTestContext { (context, db) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()

          val gameDb = db.getGame(testGame.gameId).value().value
          gameDb.roundState shouldEqual "bidding"
        }
      }

      "persists player's bid to their database record" in {
        withTestContext { (context, db) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()

          val playerDbs = db.getPlayers(testGame.gameId).value()
          val creator = playerDbs.find(_.playerId == testGame.creator.playerId.pid).value
          creator.bid shouldEqual Some(1)
        }
      }

      "fails if it is not the player's turn" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.player2, context(Fixtures.player2Address)).isFailedAttempt()
        }
      }
    }

    "for subsequent bid requests" - {
      "is successful" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          Fixtures.bid(2, testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()
          Fixtures.bid(3, testGame.player2, context(Fixtures.player2Address)).isSuccessfulAttempt()
        }
      }

      "persists player's bids to their database records" in {
        withTestContext { (context, db) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          Fixtures.bid(2, testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()
          Fixtures.bid(3, testGame.player2, context(Fixtures.player2Address)).isSuccessfulAttempt()

          val playerDbs = db.getPlayers(testGame.gameId).value()
          val bids = playerDbs.map(pdb => (pdb.playerId, pdb.bid))
          bids.toSet shouldEqual Set(
            testGame.creator.playerId.pid -> Some(1),
            testGame.player1.playerId.pid -> Some(2),
            testGame.player2.playerId.pid -> Some(3),
          )
        }
      }

      "fails if the bid is lower than an existing bid" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          Fixtures.bid(2, testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()
          Fixtures.bid(3, testGame.player2, context(Fixtures.player2Address)).isSuccessfulAttempt()
          Fixtures.bid(2, testGame.creator, context(Fixtures.creatorAddress)).isFailedAttempt()
        }
      }

      "fails if the bid is lower than a previous bid by that player" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(2, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          Fixtures.bid(3, testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()
          Fixtures.bid(4, testGame.player2, context(Fixtures.player2Address)).isSuccessfulAttempt()
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isFailedAttempt()
        }
      }

      "fails if the bid exceeds the number of discs" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(8, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
        }
      }

      "fails if it is not this player's turn" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          Fixtures.bid(2, testGame.creator, context(Fixtures.creatorAddress)).isFailedAttempt()
        }
      }

      "fails if the player has passed" in {
        withTestContext { (context, _) =>
          val testGame = goToBeforeBiddingRound(context)
          Fixtures.bid(1, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
          Fixtures.pass(testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()
          Fixtures.bid(2, testGame.player2, context(Fixtures.player2Address)).isSuccessfulAttempt()
          Fixtures.bid(3, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()

          Fixtures.bid(4, testGame.player1, context(Fixtures.player1Address)).isFailedAttempt()
        }
      }
    }
  }
}
