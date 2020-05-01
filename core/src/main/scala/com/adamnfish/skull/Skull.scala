package com.adamnfish.skull

import com.adamnfish.skull.attempt.{Attempt, Failure}
import com.adamnfish.skull.logic.{Games, Players, Representations, Responses}
import com.adamnfish.skull.models._
import com.adamnfish.skull.validation.Validation.validate

import scala.concurrent.ExecutionContext


object Skull {
  def main(requestBody: String, context: Context)(implicit ec: ExecutionContext): Attempt[Unit] = {
    val result = for {
    request <- Serialisation.decodeRequest(requestBody)
      response <- request match {
        case request: CreateGame =>
          createGame(request, context)
        case request: JoinGame =>
          joinGame(request, context)
        case request: NewRound =>
          newRound(request, context)
        case request: StartGame =>
          startGame(request, context)
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
      _ <- response.response.fold(Attempt.unit) { msg =>
        context.messaging.sendMessage(context.playerAddress, msg)(ec)
      }
      // send other messages
      _ <- Attempt.traverse(response.messages.toList) { case (address, msg) =>
        context.messaging.sendMessage(address, msg)(ec)
      }
    } yield ()
    // if there has been a failure we tell the requesting player
    result.tapFailure { failure =>
      context.messaging.sendError(context.playerAddress, failure)(ec)
    }
    result
  }

  def createGame(request: CreateGame, context: Context)(implicit ec: ExecutionContext): Attempt[Response[Welcome]] = {
    for {
      _ <- validate(request)
      creator = Players.newPlayer(request.screenName, context.playerAddress)
      game = Games.newGame(request.gameName, creator)
      gameDb = Representations.gameForDb(game)
      // TODO: check clashing gameCode doesn't already exist
      creatorDb = Representations.playerForDb(game, creator)
      _ <- context.db.writeGame(gameDb)
      _ <- context.db.writePlayer(creatorDb)
      welcome = Welcome(creator.playerKey, creator.playerId, game.gameId)
    } yield Responses.justRespond(welcome)
  }

  def joinGame(request: JoinGame, context: Context)(implicit ec: ExecutionContext): Attempt[Response[Welcome]] = {
    for {
      _ <- validate(request)
      gameDbOpt <- context.db.lookupGame(request.gameCode)
      gameDb <- Attempt.fromOption(gameDbOpt,
        Failure(
          s"Game not found for code ${request.gameCode}",
          "Couldn't find game, is the code correct?",
          404
        ).asAttempt
      )
      playerDbs <- context.db.getPlayers(GameId(gameDb.gameId))
      game <- Representations.dbToGame(gameDb, playerDbs)
      _ <- Games.ensureNotStarted(game)
      // TODO: check if this address (i.e. connection) is already in the game?
      player = Players.newPlayer(request.screenName, context.playerAddress)
      playerDb = Representations.playerForDb(game, player)
      _ <- context.db.writePlayer(playerDb)
      welcome = Welcome(player.playerKey, player.playerId, game.gameId)
    } yield Responses.justRespond(welcome)
  }

  def startGame(request: StartGame, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- Attempt.unit
      // validate request
      // ensure creator
      // get game
      // get players
      // copy players into game
      // set game to started
      // set round to <TBD>
      // save all players
      // save game
      // build status message for each player
    } yield Responses.tbd[GameStatus]
  }

  def newRound(request: NewRound, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- Attempt.unit
      // validate request
      // fetch the game and players
      // check game state is finished
      // reset the game round
      // updates the players as well
      // make messages for everyone
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
