package com.adamnfish.thorn.validation

import com.adamnfish.thorn.attempt.{Attempt, FailedAttempt, Failure}
import com.adamnfish.thorn.models._
import com.adamnfish.thorn.validation.Validators._

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
    validate(startGame.gameId.gid, "game id", isUUID) |!|
      validate(startGame.playerId.pid, "player id", isUUID) |!|
      validate(startGame.playerKey.key, "player key", isUUID) |!|
      validate(startGame.playerOrder, "player order", nonEmptyList[PlayerId]) |!|
      startGame.playerOrder
        .map(playerId => validate(playerId.pid, "player order", isUUID))
        .fold(Attempt.unit)(_ |!| _)
  }

  def validate(placeDisc: PlaceDisc)(implicit ec: ExecutionContext): Attempt[Unit] = {
    validate(placeDisc.gameId.gid, "game id", isUUID) |!|
      validate(placeDisc.playerId.pid, "player id", isUUID) |!|
      validate(placeDisc.playerKey.key, "player key", isUUID)
  }

  def validate(bid: Bid)(implicit ec: ExecutionContext): Attempt[Unit] = {
    validate(bid.gameId.gid, "game id", isUUID) |!|
      validate(bid.playerId.pid, "player id", isUUID) |!|
      validate(bid.playerKey.key, "player key", isUUID) |!|
      validate(bid.count, "count", greaterThanZero)
  }

  def validate(pass: Pass)(implicit ec: ExecutionContext): Attempt[Unit] = {
    validate(pass.gameId.gid, "game id", isUUID) |!|
      validate(pass.playerId.pid, "player id", isUUID) |!|
      validate(pass.playerKey.key, "player key", isUUID)
  }

  def validate(flip: Flip)(implicit ec: ExecutionContext): Attempt[Unit] = {
    validate(flip.gameId.gid, "game id", isUUID) |!|
      validate(flip.playerId.pid, "player id", isUUID) |!|
      validate(flip.playerKey.key, "player key", isUUID) |!|
      validate(flip.stack.pid, "stack", isUUID)
  }

  def validate(newRound: NewRound)(implicit ec: ExecutionContext): Attempt[Unit] = {
    validate(newRound.gameId.gid, "game id", isUUID) |!|
      validate(newRound.playerId.pid, "player id", isUUID) |!|
      validate(newRound.playerKey.key, "player key", isUUID)
  }

  def validate(reconnect: Reconnect)(implicit ec: ExecutionContext): Attempt[Unit] = {
    validate(reconnect.gameId.gid, "game id", isUUID) |!|
      validate(reconnect.playerId.pid, "player id", isUUID) |!|
      validate(reconnect.playerKey.key, "player key", isUUID)
  }

  def validate(ping: Ping)(implicit ec: ExecutionContext): Attempt[Unit] = {
    validate(ping.gameId.gid, "game id", isUUID) |!|
      validate(ping.playerId.pid, "player id", isUUID) |!|
      validate(ping.playerKey.key, "player key", isUUID)
  }
}
