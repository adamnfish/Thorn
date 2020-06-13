package com.adamnfish.thorn.integration

import com.adamnfish.thorn.models._
import com.adamnfish.thorn._
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec


class NewRoundTest extends AnyFreeSpec with AttemptValues with OptionValues with ThornIntegration with Journeys with TestHelpers {

  "if the round was successful" - {
    "is successful" in {
      withTestContext { (context, _) =>
        val testGame = goToRoseFlippingRound(context)
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        // flip other discs
        Fixtures.flip(
          stackId = testGame.player1.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.flip(
          stackId = testGame.player1.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()

        // the winning flip for this round
        Fixtures.flip(
          stackId = testGame.player2.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()

        Fixtures.newRound(testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
      }
    }

    "sends status messages with the new game round info (including correct first player)" in {
      withTestContext { (context, _) =>
        val testGame = goToRoseFlippingRound(context)
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        // flip other discs
        Fixtures.flip(
          stackId = testGame.player1.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.flip(
          stackId = testGame.player1.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()

        // the winning flip for this round
        Fixtures.flip(
          stackId = testGame.player2.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()

        val gameStatus = Fixtures.newRound(testGame.creator, context(Fixtures.creatorAddress)).value().messages.head._2
        gameStatus.game.round.value shouldBe a[InitialDiscsSummary]
        gameStatus.game.round.value.asInstanceOf[InitialDiscsSummary] should have(
          "activePlayer" as testGame.creator.playerId.pid,
          "initialDiscs" as Map.empty,
        )
      }
    }

    "persists the game and player changes" in {
      withTestContext { (context, db) =>
        val testGame = goToRoseFlippingRound(context)
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        // flip other discs
        Fixtures.flip(
          stackId = testGame.player1.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.flip(
          stackId = testGame.player1.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()

        // the winning flip for this round
        Fixtures.flip(
          stackId = testGame.player2.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()

        Fixtures.newRound(testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()

        val gameDb = db.getGame(testGame.gameId).value().value
        gameDb should have(
          "roundState" as "initial-discs",
          "currentPlayer" as Some(testGame.creator.playerId.pid),
          "revealedDiscs" as Map.empty,
        )
        val playerDbs = db.getPlayers(testGame.gameId).value()
        playerDbs.foreach { playerDb =>
          playerDb should have(
            "passed" as false,
            "bid" as 0,
            "placedDiscs" as Nil,
          )
        }
      }
    }
  }

  "if the player hit a Thorn" - {
    "is successful" in {
      withTestContext { (context, _) =>
        val testGame = goToThornFlippingRound(context)
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()

        // player 1 has a Thorn on top of their pile
        Fixtures.flip(
          stackId = testGame.player1.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()


        Fixtures.newRound(testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
      }
    }

    "sends status messages out with the new round information (including correct first player)" in {
      withTestContext { (context, _) =>
        val testGame = goToThornFlippingRound(context)
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()

        // player 1 has a Thorn on top of their pile
        Fixtures.flip(
          stackId = testGame.player1.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()

        val gameStatus = Fixtures.newRound(testGame.creator, context(Fixtures.creatorAddress)).value().messages.head._2
        gameStatus.game.round.value shouldBe a[InitialDiscsSummary]
        gameStatus.game.round.value.asInstanceOf[InitialDiscsSummary] should have(
          "activePlayer" as testGame.player1.playerId.pid,
          "initialDiscs" as Map.empty,
        )
      }
    }

    "persists the game and player updates" in {
      withTestContext { (context, db) =>
        val testGame = goToThornFlippingRound(context)
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()

        // player 1 has a Thorn on top of their pile
        Fixtures.flip(
          stackId = testGame.player1.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()

        Fixtures.newRound(testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()

        val gameDb = db.getGame(testGame.gameId).value().value
        gameDb should have(
          "roundState" as "initial-discs",
          "currentPlayer" as Some(testGame.player1.playerId.pid),
          "revealedDiscs" as Map.empty,
        )
        val playerDbs = db.getPlayers(testGame.gameId).value()
        playerDbs.foreach { playerDb =>
          playerDb should have(
            "passed" as false,
            "bid" as 0,
            "placedDiscs" as Nil,
          )
        }
      }
    }
  }
}
