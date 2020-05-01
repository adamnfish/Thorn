package com.adamnfish.skull

import com.adamnfish.skull.attempt.Attempt
import com.adamnfish.skull.logic.{Games, Players, Representations, Responses}
import com.adamnfish.skull.models._
import com.adamnfish.skull.validation.Validation.validate

import scala.concurrent.ExecutionContext


object Skull {
  def main(requestBody: String, context: Context, messaging: Messaging)(implicit ec: ExecutionContext): Attempt[Unit] = {
    val result = for {
      request <- Serialisation.decodeRequest(requestBody)
      response <- request match {
        case request: CreateGame =>
          createGame(request, context)
        case request: JoinGame =>
          joinGame(request, context)
        case request: NewRound =>
          newRound(request, context)
        case request: StartRound =>
          startRound(request, context)
        case request: PlaceDisc =>
          placeDisc(request, context)
        case request: Bid =>
          bid(request, context)
        case request: Pass =>
          pass(request, context)
        case request: Flip =>
          flip(request, context)
        case request: Ping =>
          ping(request, context)
        case request: Wake =>
          wake(request, context)
      }
      // send message back to the requesting player
      _ <- response.self.fold(Attempt.unit) { msg =>
        messaging.sendMessage(context.playerAddress, msg)(ec)
      }
      // send other messages
      _ <- Attempt.traverse(response.messages.toList) { case (address, msg) =>
        messaging.sendMessage(address, msg)(ec)
      }
    } yield ()
    // if there has been a failure we tell the requesting player
    result.tapFailure { failure =>
      messaging.sendError(context.playerAddress, failure)(ec)
    }
    result
  }

  def createGame(request: CreateGame, context: Context)(implicit ec: ExecutionContext): Attempt[Response[Welcome]] = {
    for {
      _ <- validate(request)
      creator = Players.newPlayer(request.screenName, context.playerAddress)
      game = Games.newGame(request.gameName, creator)
      gameDb = Representations.gameForDb(game)
      creatorDb = Representations.playerForDb(game, creator)
      _ <- context.db.writeGame(gameDb)
      _ <- context.db.writePlayer(creatorDb)
      msg = Welcome(creator.playerKey, creator.playerId, game.gameId)
    } yield Responses.justRespond(msg)
  }

  def joinGame(request: JoinGame, context: Context)(implicit ec: ExecutionContext): Attempt[Response[Welcome]] = {
    for {
      _ <- Attempt.unit
    } yield Responses.tbd[Welcome]
  }

  def newRound(request: NewRound, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- Attempt.unit
    } yield Responses.tbd[GameStatus]
  }

  def startRound(request: StartRound, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- Attempt.unit
    } yield Responses.tbd[GameStatus]
  }

  def placeDisc(request: PlaceDisc, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- Attempt.unit
    } yield Responses.tbd[GameStatus]
  }

  def bid(request: Bid, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- Attempt.unit
    } yield Responses.tbd[GameStatus]
  }

  def pass(request: Pass, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- Attempt.unit
    } yield Responses.tbd[GameStatus]
  }

  def flip(request: Flip, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- Attempt.unit
    } yield Responses.tbd[GameStatus]
  }

  def ping(request: Ping, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- Attempt.unit
    } yield Responses.tbd[GameStatus]
  }

  def wake(request: Wake, context: Context)(implicit ec: ExecutionContext): Attempt[Response[Status]] = {
    Attempt.Right {
      Responses.ok()
    }
  }
}
