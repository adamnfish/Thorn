package com.adamnfish.skull.integration

import com.adamnfish.skull.models._
import com.adamnfish.skull.{AttemptValues, TestHelpers}
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{OneInstancePerTest, OptionValues}


class BidTest extends AnyFreeSpec with AttemptValues with OptionValues
  with SkullIntegration with OneInstancePerTest with TestHelpers {

  case class TestGame(
    creator: Welcome,
    player1: Welcome,
    player2: Welcome,
    order: List[Welcome],
  )

  private def getToBiddingRound(context: PlayerAddress => Context)(implicit pos: source.Position): TestGame = {
    val creatorWelcome = Fixtures.createGame(context).value().response.value
    val player1Welcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
    val player2Welcome = Fixtures.joinGame2(creatorWelcome, context).value().response.value
    Fixtures.startGame(
      creatorWelcome,
      List(creatorWelcome, player1Welcome, player2Welcome),
      context
    ).isSuccessfulAttempt()

    // initial placements
    Fixtures.placeDisc(
      Skull, creatorWelcome, context(Fixtures.creatorAddress)
    ).isSuccessfulAttempt()
    Fixtures.placeDisc(
      Skull, player1Welcome, context(Fixtures.player1Address)
    ).isSuccessfulAttempt()
    // last initial placement will trigger placement round
    val (_, gameStatus) = Fixtures.placeDisc(
      Skull, player2Welcome, context(Fixtures.player1Address)
    ).value().messages.head

    // placing round begins
    val roundSummary = gameStatus.game.round.value
    roundSummary shouldBe a[PlacingSummary]
    val activePlayer = roundSummary.asInstanceOf[PlacingSummary].activePlayer
    val (activeWelcomeMessage, activePlayerAddress) = {
      if (creatorWelcome.playerId == activePlayer) (creatorWelcome, Fixtures.creatorAddress)
      else (player1Welcome, Fixtures.player1Address)
    }
    Fixtures.placeDisc(
      Skull, activeWelcomeMessage, context(activePlayerAddress)
    ).isSuccessfulAttempt()

    TestGame(
      creator = creatorWelcome, player1Welcome, player2Welcome,
      order = List(creatorWelcome, player1Welcome, player2Welcome) // TODO lookup first player to make this correct
    )
  }

  "for a valid request" - {
    "is successful" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        Fixtures.joinGame2(creatorWelcome, context).value().response.value
        Fixtures.startGame(creatorWelcome, List(creatorWelcome, joinWelcome), context).isSuccessfulAttempt()

        Fixtures.placeDisc(
          Skull, creatorWelcome, context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        Fixtures.placeDisc(
          Skull, joinWelcome, context(Fixtures.player1Address)
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

    "persists round change to the database" in {
      withTestContext { (context, db) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        Fixtures.startGame(
          creatorWelcome,
          List(creatorWelcome, joinWelcome),
          context
        ).isSuccessfulAttempt()

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
        Fixtures.startGame(
          creatorWelcome,
          List(creatorWelcome, joinWelcome),
          context
        ).isSuccessfulAttempt()

        Fixtures.placeDisc(
          Skull, creatorWelcome, context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        val (_, gameStatus) = Fixtures.placeDisc(
          Skull, joinWelcome, context(Fixtures.player1Address)
        ).value().messages.head

        // placing round begins
        val roundSummary = gameStatus.game.round.value
        roundSummary shouldBe a[PlacingSummary]
        val activePlayer = roundSummary.asInstanceOf[PlacingSummary].activePlayer
        val (activeWelcomeMessage, activePlayerAddress) = {
          if (creatorWelcome.playerId == activePlayer) (creatorWelcome, Fixtures.creatorAddress)
          else (joinWelcome, Fixtures.player1Address)
        }
        Fixtures.placeDisc(
          Skull, activeWelcomeMessage, context(activePlayerAddress)
        ).isSuccessfulAttempt()
      }
    }

    "persists player's disc change to the database" in {
      withTestContext { (context, db) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        val (_, startedStatus) = Fixtures.startGame(
          creatorWelcome,
          List(creatorWelcome, joinWelcome),
          context
        ).value().messages.head

        Fixtures.placeDisc(
          Rose, creatorWelcome, context(Fixtures.creatorAddress)
        ).isSuccessfulAttempt()
        val (_, gameStatus) = Fixtures.placeDisc(
          Rose, joinWelcome, context(Fixtures.player1Address)
        ).value().messages.head

        // placing round begins
        val roundSummary = gameStatus.game.round.value
        roundSummary shouldBe a[PlacingSummary]
        val activePlayer = roundSummary.asInstanceOf[PlacingSummary].activePlayer
        val (activeWelcomeMessage, activePlayerAddress) = {
          if (creatorWelcome.playerId == activePlayer) (creatorWelcome, Fixtures.creatorAddress)
          else (joinWelcome, Fixtures.player1Address)
        }
        Fixtures.placeDisc(
          Skull, activeWelcomeMessage, context(activePlayerAddress)
        ).isSuccessfulAttempt()

        val playerDbs = db.getPlayers(creatorWelcome.gameId).value()
        val activePlayerDb = playerDbs.find(_.playerId == activePlayer.pid).value

        activePlayerDb.discs shouldEqual List("skull", "rose")
      }
    }
  }

  "for invalid cases" - {
    "does not allow second disc from a single player during initial discs phase" in {
      withTestContext { (context, _) =>
        val creatorWelcome = Fixtures.createGame(context).value().response.value
        val joinWelcome = Fixtures.joinGame(creatorWelcome, context).value().response.value
        Fixtures.startGame(
          creatorWelcome,
          List(creatorWelcome, joinWelcome),
          context
        ).isSuccessfulAttempt()

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
          creatorWelcome,
          List(creatorWelcome, joinWelcome),
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
