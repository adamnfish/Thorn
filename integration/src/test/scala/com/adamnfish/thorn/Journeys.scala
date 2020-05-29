package com.adamnfish.thorn

import com.adamnfish.thorn.models.{Context, GameId, PlayerAddress, Rose, Thorn, Welcome}
import org.scalactic.source
import org.scalatest.OptionValues


trait Journeys extends AttemptValues with OptionValues {

  case class TestGame(
    gameId: GameId,
    creator: Welcome,
    player1: Welcome,
    player2: Welcome,
  )

  def goToBeforeBiddingRound(context: PlayerAddress => Context)(implicit pos: source.Position): TestGame = {
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
      Rose, creatorWelcome, context(Fixtures.creatorAddress)
    ).isSuccessfulAttempt()
    Fixtures.placeDisc(
      Rose, player1Welcome, context(Fixtures.player1Address)
    ).isSuccessfulAttempt()
    // last initial placement will trigger placement round
    Fixtures.placeDisc(
      Rose, player2Welcome, context(Fixtures.player2Address)
    ).isSuccessfulAttempt()

    // placing round begins
    Fixtures.placeDisc(
      Rose, creatorWelcome, context(Fixtures.creatorAddress)
    ).isSuccessfulAttempt()
    Fixtures.placeDisc(
      Rose, player1Welcome, context(Fixtures.player1Address)
    ).isSuccessfulAttempt()
    Fixtures.placeDisc(
      Rose, player2Welcome, context(Fixtures.player2Address)
    ).isSuccessfulAttempt()

    TestGame(
      gameId = creatorWelcome.gameId,
      creator = creatorWelcome, player1Welcome, player2Welcome,
    )
  }

  def goToRoseFlippingRound(context: PlayerAddress => Context)(implicit pos: source.Position): TestGame = {
    val testGame = goToBeforeBiddingRound(context)
    Fixtures.bid(5, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
    Fixtures.pass(testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()
    Fixtures.pass(testGame.player2, context(Fixtures.player2Address)).isSuccessfulAttempt()
    testGame
  }

  def goToThornFlippingRound(context: PlayerAddress => Context)(implicit pos: source.Position): TestGame = {
    val testGame = goToBeforeBiddingRound(context)
    Fixtures.placeDisc(
      Rose, testGame.creator, context(Fixtures.creatorAddress)
    ).isSuccessfulAttempt()
    Fixtures.placeDisc(
      Thorn, testGame.player1, context(Fixtures.player1Address)
    ).isSuccessfulAttempt()
    Fixtures.bid(1, testGame.player2, context(Fixtures.player2Address)).isSuccessfulAttempt()
    Fixtures.bid(5, testGame.creator, context(Fixtures.creatorAddress)).isSuccessfulAttempt()
    Fixtures.pass(testGame.player1, context(Fixtures.player1Address)).isSuccessfulAttempt()
    Fixtures.pass(testGame.player2, context(Fixtures.player2Address)).isSuccessfulAttempt()
    testGame
  }
}
