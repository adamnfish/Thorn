package com.adamnfish.skull.persistence

import com.adamnfish.skull.attempt.Attempt
import com.adamnfish.skull.models.{GameDB, GameId, PlayerDB}
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBAsync
import org.scanamo._
import org.scanamo.syntax._
import org.scanamo.generic.auto._

import scala.concurrent.ExecutionContext


object Database {
  // TODO: switch DB models to use PlayerId - provide
  //  implicit to allow Scanamo to use those wrapper types
  
  private val games = Table[GameDB]("games")
  private val players = Table[PlayerDB]("players")

  def getGame(gameId: GameId, client: AmazonDynamoDBAsync)(implicit ec:ExecutionContext): Attempt[Option[GameDB]] = {
//    val maybeResult = ScanamoAsync(client).exec(games.get("gameId" -> gameId))
//    maybeResult.fold[Attempt[Option[GameDB]]](Attempt.Right(None)) { result =>
//      resultToAttempt(result).map(Some(_))
//    }
    ???
  }

  def getPlayers(gameId: GameId, client: AmazonDynamoDBAsync)(implicit ec:ExecutionContext): Attempt[List[PlayerDB]] = {
//    val results = ScanamoAsync(client).exec(players.query("gameId" -> gameId))
//    Attempt.traverse(results)(resultToAttempt)
    ???
  }

  def writeGame(gameDB: GameDB, client: AmazonDynamoDBAsync)(implicit ec:ExecutionContext): Attempt[Unit] = {
//    ScanamoAsync(client).exec(table.put(playerDB))
    ???
  }

  def writePlayer(playerDB: PlayerDB, client: AmazonDynamoDBAsync)(implicit ec:ExecutionContext): Attempt[Unit] = {
//    ScanamoAsync(client).exec(players.put(playerDB))
    ???
  }

  private def resultToAttempt[A](result: Either[DynamoReadError, A]): Attempt[A] = {
    ???
  }
}
