package com.adamnfish.skull.validation

import com.adamnfish.skull.attempt.{Attempt, FailedAttempt, Failure}
import com.adamnfish.skull.models.{Bid, CreateGame, Flip, JoinGame, NewRound, Pass, Ping, PlaceDisc, StartRound}
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
    validate(createGame.gameName, "game name", nonEmpty) |!|
      validate(createGame.gameName, "game name", maxLength(25)) |!|
      validate(createGame.screenName, "screen name", nonEmpty) |!|
      validate(createGame.screenName, "screen name", maxLength(25))
  }

  def validate(joinGame: JoinGame)(implicit ec: ExecutionContext): Attempt[Unit] = {
    ???
  }

  def validate(newRound: NewRound)(implicit ec: ExecutionContext): Attempt[Unit] = {
    ???
  }

  def validate(startRound: StartRound)(implicit ec: ExecutionContext): Attempt[Unit] = {
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
