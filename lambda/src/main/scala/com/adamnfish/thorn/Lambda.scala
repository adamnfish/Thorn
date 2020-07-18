package com.adamnfish.thorn

import java.util.concurrent.Executors

import com.adamnfish.thorn.models.{Context, PlayerAddress}
import com.adamnfish.thorn.persistence.DynamoDB
import com.amazonaws.services.apigatewaymanagementapi.AmazonApiGatewayManagementApiAsyncClientBuilder
import com.amazonaws.services.dynamodbv2.{AmazonDynamoDBAsync, AmazonDynamoDBAsyncClientBuilder, AmazonDynamoDBClient}
import com.amazonaws.services.lambda.runtime.{Context => AwsContext}
import com.amazonaws.services.lambda.runtime.events.{APIGatewayV2ProxyRequestEvent, APIGatewayV2ProxyResponseEvent}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Properties


class Lambda {
  implicit private val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

  private val db = {
    val dynamoDbClient = AmazonDynamoDBAsyncClientBuilder.defaultClient()
    val maybeDb = for {
      gamesTableName <- Properties.envOrNone("GAMES_TABLE")
        .toRight("games table name not configured")
      playersTableName <- Properties.envOrNone("PLAYERS_TABLE")
        .toRight("players table name not configured")
    } yield new DynamoDB(dynamoDbClient, gamesTableName, playersTableName)
    maybeDb.fold(
      { errMsg =>
        throw new RuntimeException(errMsg)
      },
      identity
    )
  }
  val apiGatewayMessagingClient = AmazonApiGatewayManagementApiAsyncClientBuilder.defaultClient()
  val awsMessaging = new AwsMessaging(apiGatewayMessagingClient)

  def handleRequest(event: APIGatewayV2ProxyRequestEvent, awsContext: AwsContext): APIGatewayV2ProxyResponseEvent = {
    // Debugging for now
    awsContext.getLogger.log(event.getBody)
    awsContext.getLogger.log(event.getRequestContext.getConnectionId)
    awsContext.getLogger.log(event.getRequestContext.getRouteKey)

    event.getRequestContext.getRouteKey match {
      case "$connect" =>
        // ignore this for now
      case "$disconnect" =>
        // ignore this for now
      case "$default" =>
        val playerAddress = PlayerAddress(event.getRequestContext.getConnectionId)
        val thornContext = Context(playerAddress, db, awsMessaging)

        val fResult = Thorn.main(event.getBody, thornContext).asFuture
        Await.result(fResult, 5.seconds).fold(
          { failure =>
            awsContext.getLogger.log(
              s"Request failed: ${failure.logString}"
            )
          },
          identity
        )
    }

    val response = new APIGatewayV2ProxyResponseEvent()
    response.setStatusCode(200)
    response.setHeaders(Map("content-type" -> "application/json").asJava)
    response.setBody("""{"status": "ok"}""")
    response
  }
}
