package com.adamnfish.skull.integration

import com.adamnfish.skull.models._
import com.adamnfish.skull.{AttemptValues, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{OneInstancePerTest, OptionValues}


class PlaceDiscTest extends AnyFreeSpec with AttemptValues with OptionValues
  with SkullIntegration with OneInstancePerTest with TestHelpers {

  "for a valid request" - {
    "after game start" - {
      "is successful" in {
        withTestContext { (context, _) =>
          val creatorWelcome = Fixtures.createGame(context).value().response.value
          val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
          Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).isSuccessfulAttempt()

          Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()
        }
      }

      "does not change the active player" in {
        withTestContext { (context, _) =>
          val creatorWelcome = Fixtures.createGame(context).value().response.value
          val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
          Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinWelcome), context).isSuccessfulAttempt()

          val round = Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).value().messages.head._2.game.round.value
          round shouldBe a[InitialDiscsSummary]
          round.asInstanceOf[InitialDiscsSummary].activePlayer shouldEqual creatorWelcome.playerId
        }
      }

      "multiple places from different players are successful" in {
        withTestContext { (context, _) =>
          val creatorWelcome = Fixtures.createGame(context).value().response.value
          val join1Welcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
          val join2Welcome = Fixtures.joinGame2(creatorWelcome, context).value().response.value
          Fixtures.startGame(creatorWelcome, List(creatorWelcome, join1Welcome, join2Welcome), context).isSuccessfulAttempt()

          Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()
          Fixtures.placeDisc(
            Skull, join1Welcome, context(Fixtures.player1Address)
          ).isSuccessfulAttempt()
        }
      }

      "sends a game summary to every player" in {
        withTestContext { (context, _) =>
          val creatorWelcome = Fixtures.createGame(context).value().response.value
          val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
          Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).isSuccessfulAttempt()
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
          val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
          Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).value()
          val response = Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).value()

          response.response shouldEqual None
        }
      }

      "persists the game updates to the database" in {
        withTestContext { (context, db) =>
          val creatorWelcome = Fixtures.createGame(context).value().response.value
          val joinGameWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
          Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinGameWelcome), context).value()
          Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).value()
          val playerDbs = db.getPlayers(creatorWelcome.gameId).value()
          val creatorDb = playerDbs.find(_.playerId == creatorWelcome.playerId.pid).value

          creatorDb.discs shouldEqual List("skull")
        }
      }
    }

    "as last initial disc" - {
      "updates game round" in {
        withTestContext { (context, _) =>
          val creatorWelcome = Fixtures.createGame(context).value().response.value
          val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
          Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinWelcome), context).isSuccessfulAttempt()

          Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()
          val (_, gameStatus) = Fixtures.placeDisc(
            Skull, joinWelcome, context(Fixtures.player1Address)
          ).value().messages.head
          gameStatus.game.round.value shouldBe a[PlacingSummary]
        }
      }

      "uses the 'first active player' from previous round as the active player" in {
        withTestContext { (context, _) =>
          val creatorWelcome = Fixtures.createGame(context).value().response.value
          val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
          Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinWelcome), context).isSuccessfulAttempt()

          Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()
          val round = Fixtures.placeDisc(
            Skull, joinWelcome, context(Fixtures.player1Address)
          ).value().messages.head._2.game.round.value
          round shouldBe a[PlacingSummary]
          round.asInstanceOf[PlacingSummary].activePlayer shouldEqual creatorWelcome.playerId
        }
      }

      "persists round change to the database" in {
        withTestContext { (context, db) =>
          val creatorWelcome = Fixtures.createGame(context).value().response.value
          val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
          Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinWelcome), context).isSuccessfulAttempt()

          Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()
          Fixtures.placeDisc(
            Skull, joinWelcome, context(Fixtures.player1Address)
          ).isSuccessfulAttempt()

          val game = db.getGame(creatorWelcome.gameId).value().value
          game.roundState shouldEqual "placing"
        }
      }
    }

    "in place disc round" - {
      "is successful" in {
        withTestContext { (context, _) =>
          val creatorWelcome = Fixtures.createGame(context).value().response.value
          val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
          Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinWelcome), context).isSuccessfulAttempt()

          Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()
          Fixtures.placeDisc(
            Skull, joinWelcome, context(Fixtures.player1Address)
          ).isSuccessfulAttempt()

          // now at the placing round

          Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()
        }
      }

      "advances the active player" in {
        withTestContext { (context, _) =>
          val creatorWelcome = Fixtures.createGame(context).value().response.value
          val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
          Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinWelcome), context).isSuccessfulAttempt()

          Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()
          val (_, gameStatus) = Fixtures.placeDisc(
            Skull, joinWelcome, context(Fixtures.player1Address)
          ).value().messages.head

          // now at the placing round

          val round = Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).value().messages.head._2.game.round.value
          round shouldBe a[PlacingSummary]
          round.asInstanceOf[PlacingSummary].activePlayer shouldEqual joinWelcome.playerId
        }
      }

      "persists player's disc change to the database" in {
        withTestContext { (context, db) =>
          val creatorWelcome = Fixtures.createGame(context).value().response.value
          val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
          Fixtures.startGame(
            creatorWelcome, List(creatorWelcome, joinWelcome),
            context
          ).isSuccessfulAttempt()

          Fixtures.placeDisc(
            Rose, creatorWelcome, context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()
          val (_, gameStatus) = Fixtures.placeDisc(
            Rose, joinWelcome, context(Fixtures.player1Address)
          ).value().messages.head

          // now at placing round

          Fixtures.placeDisc(
            Skull, creatorWelcome, context(Fixtures.creatorAddress)
          ).isSuccessfulAttempt()

          val playerDbs = db.getPlayers(creatorWelcome.gameId).value()
          val creatorDb = playerDbs.find(_.playerId == creatorWelcome.playerId.pid).value

          creatorDb.discs shouldEqual List("skull", "rose")
        }
      }
    }
  }

  "for invalid cases" - {
    "does not allow second disc from a single player during initial discs phase" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinWelcome), context).isSuccessfulAttempt()

        Fixtures.placeDisc(
          Skull, creatorWelcome, context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.placeDisc(
          Skull, creatorWelcome, context(Fixtures.creatorAddress)
        ).isFailedAttempt()
      }
    }

    "does not allow a non-active player to place disc during place disc round" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        val (_, startedStatus) = Fixtures.startGame(
          creatorWelcome, List(creatorWelcome, joinWelcome),
          context
        ).value().messages.head

        Fixtures.placeDisc(
          Skull, creatorWelcome, context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.placeDisc(
          Skull, joinWelcome, context(Fixtures.player1Address)
        ).isSuccessfulAttempt()

        // placing round begins
        val activePlayer = startedStatus.game.round.value.asInstanceOf[InitialDiscsSummary].activePlayer
        val (nonActiveWelcomeMessage, nonActivePlayerAddress) = {
          if (creatorWelcome.playerId == activePlayer) (joinWelcome, Fixtures.player1Address)
          else (creatorWelcome, Fixtures.creatorAddress)
        }
        Fixtures.placeDisc(
          Skull, nonActiveWelcomeMessage, context(nonActivePlayerAddress)
        ).isFailedAttempt()
      }
    }

    "add cases" ignore {}
    // TODO:
  }
}
