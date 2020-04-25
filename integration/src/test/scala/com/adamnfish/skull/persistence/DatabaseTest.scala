package com.adamnfish.skull.persistence

import java.time.{ZoneId, ZonedDateTime}

import com.adamnfish.skull.AttemptValues
import com.adamnfish.skull.models.{GameDB, GameId, PlayerDB}
import com.amazonaws.services.dynamodbv2.model.ScalarAttributeType._
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scanamo.LocalDynamoDB

import scala.concurrent.ExecutionContext.Implicits.global


class DatabaseTest extends AnyFreeSpec with AttemptValues with OptionValues {

  val client = LocalDynamoDB.client()
  LocalDynamoDB.createTable(client)("games")("gameId" -> S)
  LocalDynamoDB.createTable(client)("players")("gameId" -> S, "playerId" -> S)

  "games table" - {
    "round trips correctly" in {
      val gameDb = GameDB(
        "game-id", List("player-1", "player-2"), true,
        ZonedDateTime.of(2020, 4, 24, 19, 52, 0, 0, ZoneId.of("UTC")),
        "flip", Some("player-1"), Map("player-1" -> 1, "player-2" -> 2)
      )

      Database.writeGame(gameDb, client).isSuccessfulAttempt()
      val result = Database.getGame(GameId(gameDb.gameId), client).value()
      result.value shouldEqual gameDb
    }
  }

  "players table" - {
    "round trips correctly" in {
      val playerDb1 = PlayerDB(
        "game-id", "player-1", "key-1", "player-1-address", "Player 1",
        2, List("skull", "rose"), Some(1), Some(true)
      )
      val playerDb2 = PlayerDB(
        "game-id", "player-2", "key-2", "player-2-address", "Player 2",
        0, List("rose"), None, Some(false)
      )

      Database.writePlayer(playerDb1, client).isSuccessfulAttempt()
      Database.writePlayer(playerDb2, client).isSuccessfulAttempt()
      val result = Database.getPlayers(GameId("game-id"), client).value()
      result shouldEqual List(playerDb1, playerDb2)
    }
  }
}
