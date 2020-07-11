package com.adamnfish.thorn

import com.adamnfish.thorn.models.{GameDB, PlayerDB}
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBAsync
import com.amazonaws.services.dynamodbv2.model.ScalarAttributeType._
import org.scanamo.{LocalDynamoDB, Table}
import org.scanamo.generic.auto._


object DevServerDB {
  def createGamesTable(client: AmazonDynamoDBAsync): Unit = {
    LocalDynamoDB.createTable(client)("games")(
      "gameCode" -> S,
      "gameId" -> S,
    )
  }

  def createPlayersTable(client: AmazonDynamoDBAsync): Unit = {
    LocalDynamoDB.createTable(client)("players")(
      "gameId" -> S,
      "playerId" -> S,
    )
  }
}
