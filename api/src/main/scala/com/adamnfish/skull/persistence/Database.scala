package com.adamnfish.skull.persistence

import com.adamnfish.skull.attempt.{Attempt, Failure}
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
    for {
      maybeResult <- execAsAttempt(client, games.get("gameId" -> gameId.gid))
      maybeGameDb <- maybeResult.fold[Attempt[Option[GameDB]]](Attempt.Right(None)) { result =>
        resultToAttempt(result).map(Some(_))
      }
    } yield maybeGameDb
  }

  def getPlayers(gameId: GameId, client: AmazonDynamoDBAsync)(implicit ec:ExecutionContext): Attempt[List[PlayerDB]] = {
    for {
      results <- execAsAttempt(client, players.query("gameId" -> gameId.gid))
      players <- Attempt.traverse(results)(resultToAttempt)
    } yield players
  }

  def writeGame(gameDB: GameDB, client: AmazonDynamoDBAsync)(implicit ec:ExecutionContext): Attempt[Unit] = {
    for {
      result <- execAsAttempt(client, games.put(gameDB))
    } yield result
  }

  def writePlayer(playerDB: PlayerDB, client: AmazonDynamoDBAsync)(implicit ec:ExecutionContext): Attempt[Unit] = {
    for {
      result <- execAsAttempt(client, players.put(playerDB))
    } yield result
  }

  def execAsAttempt[A](client: AmazonDynamoDBAsync, op: ops.ScanamoOps[A])(implicit ec:ExecutionContext): Attempt[A] = {
    Attempt.fromFuture(
      ScanamoAsync(client).exec(op)
    ) {
      case err =>
        Failure("Uncaught DB error", "I had a problem saving the game", 500, None, Some(err)).asAttempt
    }
  }

  private def resultToAttempt[A](result: Either[DynamoReadError, A]): Attempt[A] = {
    Attempt.fromEither {
      result.left.map { dre =>
        Failure(s"DynamoReadError: $dre", "Error with saved data", 500, None, None).asAttempt
      }
    }
  }
//  private implicit val gameDBFormat: DynamoFormat[GameDB] = deriveDynamoFormat[GameDB]
//  private implicit val playerDBFormat: DynamoFormat[PlayerDB] = deriveDynamoFormat[PlayerDB]
}
