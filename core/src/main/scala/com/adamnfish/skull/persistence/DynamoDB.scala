package com.adamnfish.skull.persistence

import com.adamnfish.skull.attempt.{Attempt, Failure}
import com.adamnfish.skull.logic.Games
import com.adamnfish.skull.models.{GameDB, GameId, PlayerDB}
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBAsync
import org.scanamo._
import org.scanamo.syntax._
import org.scanamo.generic.auto._
import org.scanamo.query.BeginsWith

import scala.concurrent.ExecutionContext


class DynamoDB(client: AmazonDynamoDBAsync) extends Database {
  // TODO: switch DB models to use PlayerId - provide
  //  implicit to allow Scanamo to use those wrapper types

  private val games = Table[GameDB]("games")
  private val players = Table[PlayerDB]("players")

  // TODO: consider whether this should just derive a gameCode and call lookup
  override def getGame(gameId: GameId)(implicit ec:ExecutionContext): Attempt[Option[GameDB]] = {
    val gameCode = Games.gameCode(gameId)
    for {
      maybeResult <- execAsAttempt(games.get("gameCode" -> gameCode and "gameId" -> gameId.gid))
//      maybeResult <- results match {
//        case Nil =>
//          Attempt.Right(None)
//        case result :: Nil =>
//          Attempt.Right(Some(result))
//        case _ =>
//          Attempt.Left(
//            Failure(
//              s"Multiple games found for id `${gameId.gid}`",
//              "Couldn't find a game for that code",
//              409
//            )
//          )
//      }
      maybeGameDb <- maybeResult.fold[Attempt[Option[GameDB]]](Attempt.Right(None)) { result =>
        resultToAttempt(result).map(Some(_))
      }
    } yield maybeGameDb
  }


  override def lookupGame(gameCode: String)(implicit ec: ExecutionContext): Attempt[Option[GameDB]] = {
    for {
      results <- execAsAttempt(games.query("gameCode" -> gameCode and ("gameId" beginsWith gameCode)))
      maybeResult <- results match {
        case Nil =>
          Attempt.Right(None)
        case result :: Nil =>
          Attempt.Right(Some(result))
        case _ =>
          Attempt.Left(
            Failure(
              s"Multiple games found for code `$gameCode`",
              "Couldn't find a game for that code",
              409
            )
          )
      }
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
}
