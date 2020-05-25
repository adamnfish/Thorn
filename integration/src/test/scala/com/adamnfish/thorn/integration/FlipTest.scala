package com.adamnfish.thorn.integration

import com.adamnfish.thorn.models._
import com.adamnfish.thorn.{AttemptValues, TestHelpers}
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec


class FlipTest extends AnyFreeSpec with AttemptValues with OptionValues
  with ThornIntegration with TestHelpers with Journeys {

  "at the start of the flipping round" - {
    "is successful when flipping their first discs" in {
      withTestContext { (context, _) =>
        val testGame = goToRoseFlippingRound(context)
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
      }
    }

    "sends a message to every player" in {
      withTestContext { (context, _) =>
        val testGame = goToRoseFlippingRound(context)
        val messages = Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).value().messages
        messages.keys.toSet shouldEqual Set(Fixtures.creatorAddress, Fixtures.player1Address, Fixtures.player2Address)
      }
    }

    "includes the newly revealed disc in the game summary message" in {
      withTestContext { (context, _) =>
        val testGame = goToRoseFlippingRound(context)
        val messages = Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).value().messages
        val round = messages.head._2.game.round.value
        round shouldBe a[FlippingSummary]
        round.asInstanceOf[FlippingSummary].revealed shouldEqual Map(
          testGame.creator.playerId -> List(Rose),
          testGame.player1.playerId -> Nil,
          testGame.player2.playerId -> Nil,
        )
      }
    }

    "doesn't return a response message" in {
      withTestContext { (context, _) =>
        val testGame = goToRoseFlippingRound(context)
        val response = Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).value()
        response.response shouldEqual None
      }
    }

    "is successful when flipping all their own discs" in {
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
      }
    }

    "persists a flip to the database" in {
      withTestContext { (context, db) =>
        val testGame = goToRoseFlippingRound(context)
        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()

        val gameDb = db.getGame(testGame.gameId).value().value
        gameDb.revealedDiscs shouldEqual Map(
          testGame.creator.playerId.pid -> 1,
          testGame.player1.playerId.pid -> 0,
          testGame.player2.playerId.pid -> 0,
        )
      }
    }

    "persists multiple flips to the database" in {
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

        val gameDb = db.getGame(testGame.gameId).value().value
        gameDb.revealedDiscs shouldEqual Map(
          testGame.creator.playerId.pid -> 2,
          testGame.player1.playerId.pid -> 0,
          testGame.player2.playerId.pid -> 0,
        )
      }
    }

    "fails to flip another player's disc" in {
      withTestContext { (context, _) =>
        val testGame = goToRoseFlippingRound(context)
        Fixtures.flip(
          stackId = testGame.player1.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isFailedAttempt()
      }
    }
  }

  "after flipping their own discs" - {
    "can flip another player's Rose" - {
      "successfully" in {
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

          Fixtures.flip(
            stackId = testGame.player1.playerId,
            testGame.creator,
            context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()
        }
      }

      "and the revealed state is persisted" in {
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

          Fixtures.flip(
            stackId = testGame.player1.playerId,
            testGame.creator,
            context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()

          val gameDb = db.getGame(testGame.gameId).value().value
          gameDb.revealedDiscs shouldEqual Map(
            testGame.creator.playerId.pid -> 2,
            testGame.player1.playerId.pid -> 1,
            testGame.player2.playerId.pid -> 0,
          )
        }
      }

      "if this Rose flip meets the round target" - {
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
            val gameStatus = Fixtures.flip(
              stackId = testGame.player2.playerId,
              testGame.creator,
              context(Fixtures.creatorAddress)
            ).value().messages.get(Fixtures.creatorAddress).value
            gameStatus.game.round.value shouldBe a[FinishedSummary]
            val round = gameStatus.game.round.value.asInstanceOf[FinishedSummary]
            round.successful shouldEqual true
          }
        }

        "persists the round to end to the database" in {
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

            val gameDb = db.getGame(testGame.gameId).value().value
            gameDb.roundState shouldEqual "finished"
          }
        }

        "updates the player's score in the returned message" in {
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
            val gameStatus = Fixtures.flip(
              stackId = testGame.player2.playerId,
              testGame.creator,
              context(Fixtures.creatorAddress)
            ).value().messages.get(Fixtures.creatorAddress).value
            // game summary
            val updatedCreator = gameStatus.game.players.find(_.playerId == testGame.creator.playerId).value
            updatedCreator.score shouldEqual 1
            // self summary
            gameStatus.self.score shouldEqual 1
          }
        }

        "persists the player's score change to the database" in {
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

            val playerDbs = db.getPlayers(testGame.gameId).value()
            val creatorDb = playerDbs.find(_.playerId == testGame.creator.playerId.pid).value
            creatorDb.score shouldEqual 1
          }
        }
      }
    }

    "can flip another player's Thorn" - {
      "successfully" in {
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

          // player 1 has a Thorn on top of their stack
          Fixtures.flip(
            stackId = testGame.player1.playerId,
            testGame.creator,
            context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()
        }
      }

      "the returned message is a correct finished round" in {
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

          // player 1 has a Thorn on top of their stack
          val messages = Fixtures.flip(
            stackId = testGame.player1.playerId,
            testGame.creator,
            context(Fixtures.creatorAddress)
          ).value().messages
          val round = messages.head._2.game.round.value
          round shouldBe a[FinishedSummary]
          round.asInstanceOf[FinishedSummary] should have(
            "activePlayer" as testGame.creator.playerId.pid,
            "discs" as Map(
              testGame.creator.playerId -> 3,
              testGame.player1.playerId -> 3,
              testGame.player2.playerId -> 2,
            ),
            "revealed" as Map(
              testGame.creator.playerId -> List(Rose, Rose, Rose),
              testGame.player1.playerId -> List(Thorn),
              testGame.player2.playerId -> Nil,
            ),
            "successful" as false,
          )
        }
      }

      "the returned message shows the player has had a disc removed from their pool" in {
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

          // player 1 has a Thorn on top of their stack
          val message = Fixtures.flip(
            stackId = testGame.player1.playerId,
            testGame.creator,
            context(Fixtures.creatorAddress)
          ).value().messages.head._2
          val creatorSummary = message.game.players.find(_.playerId == testGame.creator.playerId).value
          creatorSummary.discCount shouldEqual 3
        }
      }

      "the player's self summary shows the player has had a disc removed from their pool" in {
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

          // player 1 has a Thorn on top of their stack
          val message = Fixtures.flip(
            stackId = testGame.player1.playerId,
            testGame.creator,
            context(Fixtures.creatorAddress)
          ).value().messages.head._2

          val thornRemoved = !message.self.hasThorn
          val roseRemoved = message.self.roseCount == 2
          withClue(s"Expect one removed of Thorn:$thornRemoved Rose:$roseRemoved") {
            (thornRemoved != roseRemoved) shouldEqual true
          }
        }
      }

      "finished state is persisted to the database" in {
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

          // player 1 has a Thorn on top of their stack
          Fixtures.flip(
            stackId = testGame.player1.playerId,
            testGame.creator,
            context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()

          val gameDb = db.getGame(testGame.gameId).value().value
          gameDb.roundState shouldEqual "finished"
          gameDb.revealedDiscs shouldEqual Map(
            testGame.creator.playerId.pid -> 3,
            testGame.player1.playerId.pid -> 1,
            testGame.player2.playerId.pid -> 0,
          )
        }
      }

      "player's disc removal is persisted to the database" in {
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

          // player 1 has a Thorn on top of their stack
          Fixtures.flip(
            stackId = testGame.player1.playerId,
            testGame.creator,
            context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()

          val playerDbs = db.getPlayers(testGame.gameId).value()
          val creatorDb = playerDbs.find(_.playerId == testGame.creator.playerId.pid).value
          val thornRemoved = !creatorDb.hasThorn
          val roseRemoved = creatorDb.roseCount == 2
          withClue(s"Expect one removed of Thorn:$thornRemoved Rose:$roseRemoved") {
            (thornRemoved != roseRemoved) shouldEqual true
          }
        }
      }
    }

    "fails to flip beyond their placed discs" in {
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

        Fixtures.flip(
          stackId = testGame.creator.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isFailedAttempt()
      }
    }

    "cannot keep flipping another player's discs beyond what they have placed" in {
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

        Fixtures.flip(
          stackId = testGame.player1.playerId,
          testGame.creator,
          context(Fixtures.creatorAddress)
        ).isFailedAttempt()
      }
    }
  }
}
