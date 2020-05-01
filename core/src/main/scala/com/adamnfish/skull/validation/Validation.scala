package com.adamnfish.skull.validation

import com.adamnfish.skull.attempt.{Attempt, FailedAttempt, Failure}
import com.adamnfish.skull.models.{Bid, CreateGame, Flip, JoinGame, NewRound, Pass, Ping, PlaceDisc, StartGame}
import com.adamnfish.skull.validation.Validators._

import scala.concurrent.ExecutionContext


object Validation {
  type Validator[A] = (A, String) => List[Failure]

  private[validation] def validate[A](a: A, context: String, validator: Validator[A]): Attempt[Unit] = {
    val failures = validator(a, context)
    if (failures.isEmpty) Attempt.unit
    else Attempt.Left(FailedAttempt(failures))
  }

  def validate(createGame: CreateGame)(implicit ec: ExecutionContext): Attempt[Unit] = {
    validate(createGame.gameName, "game name", sensibleLength) |!|
      validate(createGame.screenName, "screen name", sensibleLength)
  }

  def validate(joinGame: JoinGame)(implicit ec: ExecutionContext): Attempt[Unit] = {
    validate(joinGame.gameCode, "game code", gameCode) |!|
      validate(joinGame.screenName, "screen name", sensibleLength)
  }

  def validate(startGame: StartGame)(implicit ec: ExecutionContext): Attempt[Unit] = {
    ???
  }

  def validate(newRound: NewRound)(implicit ec: ExecutionContext): Attempt[Unit] = {
    ???
  }

  def validate(placeDisc: PlaceDisc)(implicit ec: ExecutionContext): Attempt[Unit] = {
    ???
  }

  def validate(bid: Bid)(implicit ec: ExecutionContext): Attempt[Unit] = {
    ???
  }

  def validate(pass: Pass)(implicit ec: ExecutionContext): Attempt[Unit] = {
    ???
  }

  def validate(flip: Flip)(implicit ec: ExecutionContext): Attempt[Unit] = {
    ???
  }

  def validate(ping: Ping)(implicit ec: ExecutionContext): Attempt[Unit] = {
    ???
  }
}
