package com.adamnfish.thorn

import com.adamnfish.thorn.attempt.{Attempt, Failure}
import com.adamnfish.thorn.logic.{Games, Play, Players, Representations, Responses}
import com.adamnfish.thorn.models._
import com.adamnfish.thorn.validation.Validation.validate

import scala.concurrent.ExecutionContext


object Thorn {
  def main(requestBody: String, context: Context)(implicit ec: ExecutionContext): Attempt[Unit] = {
    val result = for {
    request <- Serialisation.decodeRequest(requestBody)
      response <- request match {
        case request: CreateGame =>
          createGame(request, context)
        case request: JoinGame =>
          joinGame(request, context)
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
        case request: NewRound =>
          newRound(request, context)
        case request: Reconnect =>
          reconnect(request, context)
        case request: Ping =>
          ping(request, context)
        case request: Wake =>
          wake(request, context)
      }
      // send message back to the requesting player
      _ <- response.response.fold(Attempt.unit) { msg =>
        context.messaging.sendMessage(context.playerAddress, msg)
      }
      // send other messages
      _ <- Attempt.traverse(response.messages.toList) { case (address, msg) =>
        context.messaging.sendMessage(address, msg)
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
      creatorDb = Representations.newPlayerForDb(game, creator)
      _ <- context.db.writeGame(gameDb)
      _ <- context.db.writePlayer(creatorDb)
      welcome = Welcome(creator.playerKey, creator.playerId, game.gameId)
    } yield Responses.justRespond(welcome)
  }

  def joinGame(request: JoinGame, context: Context)(implicit ec: ExecutionContext): Attempt[Response[Welcome]] = {
    for {
      _ <- validate(request)
      // fetch game data
      gameDbOpt <- context.db.lookupGame(request.gameCode)
      rawGameDb <- Attempt.fromOption(gameDbOpt,
        Failure(
          s"Game not found for code ${request.gameCode}",
          "Couldn't find game, is the code correct?",
          404
        ).asAttempt
      )
      playerDbs <- context.db.getPlayers(GameId(rawGameDb.gameId))
      // game logic
      gameDb = Games.addPlayerIds(rawGameDb, playerDbs)
      game <- Representations.dbToGame(gameDb, playerDbs)
      _ <- Games.ensureNotStarted(game)
      _ <- Games.ensureNotAlreadyPlaying(game, context.playerAddress)
      _ <- Games.ensureNoDuplicateScreenName(game, request.screenName)
      player = Players.newPlayer(request.screenName, context.playerAddress)
      welcome = Welcome(player.playerKey, player.playerId, game.gameId)
      newGame = Games.addPlayer(player, game)
      response <- Responses.messageAndStatuses(welcome, newGame)
      // create and save new DB representations
      playerDb = Representations.newPlayerForDb(newGame, player)
      _ <- context.db.writePlayer(playerDb)
    } yield response
  }

  def startGame(request: StartGame, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- validate(request)
      // fetch game / player data
      gameDbOpt <- context.db.getGame(request.gameId)
      rawGameDb <- Games.requireGame(gameDbOpt, request.gameId.gid)
      playerDbs <- context.db.getPlayers(request.gameId)
      // game logic
      gameDb = Games.addOrderedPlayerIds(rawGameDb, request.playerOrder)
      game <- Representations.dbToGame(gameDb, playerDbs)
      _ <- Games.ensurePlayerKey(game, request.playerId, request.playerKey)
      _ <- Games.ensureCreator(request.playerId, game)
      _ <- Games.ensureNotStarted(game)
      // TODO: enforce minimum player count
      newGame = Games.startGame(game)
      response <- Responses.gameStatuses(newGame)
      // create and save new DB representations
      newGameDb = Representations.gameForDb(newGame)
      _ <- context.db.writeGame(newGameDb)
    } yield response
  }

  def placeDisc(request: PlaceDisc, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- validate(request)
      // fetch game / player data
      gameDbOpt <- context.db.getGame(request.gameId)
      gameDb <- Games.requireGame(gameDbOpt, request.gameId.gid)
      playerDbs <- context.db.getPlayers(request.gameId)
      game <- Representations.dbToGame(gameDb, playerDbs)
      // check player
      _ <- Games.ensurePlayerKey(game, request.playerId, request.playerKey)
      // game logic
      newGame <- Play.placeDisc(request.disc, request.playerId, game)
      response <- Responses.gameStatuses(newGame)
      // create and save updated player and game for DB
      newPlayerDb <- Representations.playerForDb(newGame, request.playerId)
      newGameDb = Representations.gameForDb(newGame)
      _ <- context.db.writePlayer(newPlayerDb)
      _ <- context.db.writeGame(newGameDb)
    } yield response
  }

  def bid(request: Bid, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- validate(request)
      // fetch game / player data
      gameDbOpt <- context.db.getGame(request.gameId)
      gameDb <- Games.requireGame(gameDbOpt, request.gameId.gid)
      playerDbs <- context.db.getPlayers(request.gameId)
      game <- Representations.dbToGame(gameDb, playerDbs)
      // check player
      _ <- Games.ensurePlayerKey(game, request.playerId, request.playerKey)
      // game logic
      newGame <- Play.bidOnRound(request.count, request.playerId, game)
      response <- Responses.gameStatuses(newGame)
      // create and save updated player and game for DB
      newPlayerDb <- Representations.playerForDb(newGame, request.playerId)
      newGameDb = Representations.gameForDb(newGame)
      _ <- context.db.writePlayer(newPlayerDb)
      _ <- context.db.writeGame(newGameDb)
    } yield response
  }

  def pass(request: Pass, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- validate(request)
      // fetch player / game data
      gameDbOpt <- context.db.getGame(request.gameId)
      gameDb <- Games.requireGame(gameDbOpt, request.gameId.gid)
      playerDbs <- context.db.getPlayers(request.gameId)
      game <- Representations.dbToGame(gameDb, playerDbs)
      // check player
      _ <- Games.ensurePlayerKey(game, request.playerId, request.playerKey)
      // game logic
      newGame <- Play.passRound(request.playerId, game)
      response <- Responses.gameStatuses(newGame)
      // create and save updated player for DB
      newPlayerDb <- Representations.playerForDb(newGame, request.playerId)
      newGameDb = Representations.gameForDb(newGame)
      _ <- context.db.writePlayer(
        newPlayerDb.copy(passed = true)
      )
      _ <- context.db.writeGame(newGameDb)
    } yield response
  }

  def flip(request: Flip, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- validate(request)
      // fetch player / game data
      gameDbOpt <- context.db.getGame(request.gameId)
      gameDb <- Games.requireGame(gameDbOpt, request.gameId.gid)
      playerDbs <- context.db.getPlayers(request.gameId)
      game <- Representations.dbToGame(gameDb, playerDbs)
      // check player
      _ <- Games.ensurePlayerKey(game, request.playerId, request.playerKey)
      // game logic
      newGame <- Play.flipDisc(request.playerId, request.stack, game)
      response <- Responses.gameStatuses(newGame)
      // create and save updated player and game for DB
      newGameDb = Representations.gameForDb(newGame)
      newPlayerDb <- Representations.playerForDb(newGame, request.playerId)
      _ <- context.db.writePlayer(newPlayerDb)
      _ <- context.db.writeGame(newGameDb)
    } yield response
  }

  def newRound(request: NewRound, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- validate(request)
      // fetch player / game data
      gameDbOpt <- context.db.getGame(request.gameId)
      gameDb <- Games.requireGame(gameDbOpt, request.gameId.gid)
      playerDbs <- context.db.getPlayers(request.gameId)
      game <- Representations.dbToGame(gameDb, playerDbs)
      // check player
      _ <- Games.ensurePlayerKey(game, request.playerId, request.playerKey)
      // reset the game round
      newGame <- Play.newRound(game)
      // make messages for everyone
      response <- Responses.gameStatuses(newGame)
      // updates the players as well - un-passes, empties discs etc
      newGameDb = Representations.gameForDb(newGame)
      newPlayerDbs = Representations.playersForDb(newGame)
      _ <- Attempt.traverse(newPlayerDbs)(context.db.writePlayer)
      _ <- context.db.writeGame(newGameDb)
    } yield response
  }

  def ping(request: Ping, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- validate(request)
      // fetch player / game data
      gameDbOpt <- context.db.getGame(request.gameId)
      gameDb <- Games.requireGame(gameDbOpt, request.gameId.gid)
      playerDbs <- context.db.getPlayers(request.gameId)
      game <- Representations.dbToGame(gameDb, playerDbs)
      // check player
      _ <- Games.ensurePlayerKey(game, request.playerId, request.playerKey)
      // game data for response
      gameStatus <- Representations.gameStatus(game, request.playerId)
    } yield Responses.justRespond(gameStatus)
  }

  def wake(request: Wake, context: Context)(implicit ec: ExecutionContext): Attempt[Response[Status]] = {
    Attempt.Right {
      Responses.ok()
    }
  }

  /**
   * Essentially, the job here is to update the player's address.
   * More sophisticated connect / disconnect / reconnect behaviour would be great at some point.
   */
  def reconnect(request: Reconnect, context: Context)(implicit ec: ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      _ <- validate(request)
      // fetch player / game data
      gameDbOpt <- context.db.getGame(request.gameId)
      gameDb <- Games.requireGame(gameDbOpt, request.gameId.gid)
      playerDbs <- context.db.getPlayers(request.gameId)
      game <- Representations.dbToGame(gameDb, playerDbs)
      // check player
      _ <- Games.ensurePlayerKey(game, request.playerId, request.playerKey)
      message <- Representations.gameStatus(game, request.playerId)
      // logic
      newGame <- Games.updatePlayerAddress(request.playerId, context.playerAddress, game)
      // create and save updated player for DB
      newPlayerDb <- Representations.playerForDb(newGame, request.playerId)
      _ <- context.db.writePlayer(newPlayerDb)
    } yield Responses.justRespond(message)
  }
}
