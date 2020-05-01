package com.adamnfish.skull.persistence

import com.adamnfish.skull.attempt.{Attempt, Failure}
import com.adamnfish.skull.models.{GameDB, GameId, PlayerDB}
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBAsync
import org.scanamo._
import org.scanamo.syntax._
import org.scanamo.generic.auto._

import scala.concurrent.ExecutionContext


class DynamoDB(client: AmazonDynamoDBAsync) extends Database {
  // TODO: switch DB models to use PlayerId - provide
  //  implicit to allow Scanamo to use those wrapper types

  private val games = Table[GameDB]("games")
  private val players = Table[PlayerDB]("players")

  override def getGame(gameId: GameId)(implicit ec:ExecutionContext): Attempt[Option[GameDB]] = {
    for {
      maybeResult <- execAsAttempt(games.get("gameId" -> gameId.gid))
      maybeGameDb <- maybeResult.fold[Attempt[Option[GameDB]]](Attempt.Right(None)) { result =>
        resultToAttempt(result).map(Some(_))
      }
    } yield maybeGameDb
  }

  override def getPlayers(gameId: GameId)(implicit ec:ExecutionContext): Attempt[List[PlayerDB]] = {
    for {
      results <- execAsAttempt(players.query("gameId" -> gameId.gid))
      players <- Attempt.traverse(results)(resultToAttempt)
    } yield players
  }

  override def writeGame(gameDB: GameDB)(implicit ec:ExecutionContext): Attempt[Unit] = {
    for {
      result <- execAsAttempt(games.put(gameDB))
    } yield result
  }

  override def writePlayer(playerDB: PlayerDB)(implicit ec:ExecutionContext): Attempt[Unit] = {
    for {
      result <- execAsAttempt(players.put(playerDB))
    } yield result
  }

  def execAsAttempt[A](op: ops.ScanamoOps[A])(implicit ec:ExecutionContext): Attempt[A] = {
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
