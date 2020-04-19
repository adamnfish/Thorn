package com.adamnfish.skull

import com.adamnfish.skull.attempt.Attempt
import com.adamnfish.skull.models._

import scala.concurrent.ExecutionContext


object Skull {
  def createGame(request: CreateGame, context: Context)(implicit ec: ExecutionContext): Attempt[Messages] = {
    for {
      _ <- Attempt.unit
    } yield Messages.empty
  }

  def joinGame(request: JoinGame, context: Context)(implicit ec: ExecutionContext): Attempt[Messages] = {
    for {
      _ <- Attempt.unit
    } yield Messages.empty
  }

  def newRound(request: NewRound, context: Context)(implicit ec: ExecutionContext): Attempt[Messages] = {
    for {
      _ <- Attempt.unit
    } yield Messages.empty
  }

  def startRound(request: StartRound, context: Context)(implicit ec: ExecutionContext): Attempt[Messages] = {
    for {
      _ <- Attempt.unit
    } yield Messages.empty
  }

  def placeDisc(request: PlaceDisc, context: Context)(implicit ec: ExecutionContext): Attempt[Messages] = {
    for {
      _ <- Attempt.unit
    } yield Messages.empty
  }

  def bid(request: Bid, context: Context)(implicit ec: ExecutionContext): Attempt[Messages] = {
    for {
      _ <- Attempt.unit
    } yield Messages.empty
  }

  def pass(request: Pass, context: Context)(implicit ec: ExecutionContext): Attempt[Messages] = {
    for {
      _ <- Attempt.unit
    } yield Messages.empty
  }

  def flip(request: Flip, context: Context)(implicit ec: ExecutionContext): Attempt[Messages] = {
    for {
      _ <- Attempt.unit
    } yield Messages.empty
  }

  def ping(request: Ping, context: Context)(implicit ec: ExecutionContext): Attempt[Messages] = {
    for {
      _ <- Attempt.unit
    } yield Messages.empty
  }

  def wake(request: Wake, context: Context)(implicit ec: ExecutionContext): Attempt[Messages] = {
    for {
      _ <- Attempt.unit
    } yield Messages.empty
  }
}
