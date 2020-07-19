package com.adamnfish.thorn

import java.util.concurrent.Executors

import com.adamnfish.thorn.models.{Context, PlayerAddress}
import com.adamnfish.thorn.persistence.DynamoDB
import com.amazonaws.client.builder.AwsClientBuilder
import com.amazonaws.client.builder.AwsClientBuilder.EndpointConfiguration
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
  val apiGatewayMessagingClient = {
    val maybeApiGatewayClient = for {
      apiGatewayEndpoint <- Properties.envOrNone("API_ORIGIN_LOCATION")
        .toRight("API Gateway endpoint name not configured")
      region <- Properties.envOrNone("REGION")
        .toRight("region not configured")
    } yield {
      val endpointConfig = new EndpointConfiguration(apiGatewayEndpoint, region)
      AmazonApiGatewayManagementApiAsyncClientBuilder.standard()
        .withEndpointConfiguration(endpointConfig)
        .build()
    }
    maybeApiGatewayClient.fold(
      { errMsg =>
        throw new RuntimeException(errMsg)
      },
      identity
    )
  }

  def handleRequest(event: APIGatewayV2ProxyRequestEvent, awsContext: AwsContext): APIGatewayV2ProxyResponseEvent = {
    val awsMessaging = new AwsMessaging(apiGatewayMessagingClient, awsContext.getLogger)
    // Debugging for now
    awsContext.getLogger.log(s"request body: ${event.getBody}")
    awsContext.getLogger.log(s"connection ID: ${event.getRequestContext.getConnectionId}")
    awsContext.getLogger.log(s"route: ${event.getRequestContext.getRouteKey}")

    event.getRequestContext.getRouteKey match {
      case "$connect" =>
        // ignore this for now
      case "$disconnect" =>
        // ignore this for now
      case "$default" =>
        val playerAddress = PlayerAddress(event.getRequestContext.getConnectionId)
        val thornContext = Context(playerAddress, db, awsMessaging)

        val fResult = Thorn.main(event.getBody, thornContext).asFuture
        Await.result(fResult, 10.seconds).fold(
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
