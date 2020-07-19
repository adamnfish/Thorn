package com.adamnfish.thorn
import java.nio.ByteBuffer

import com.adamnfish.thorn.attempt.{Attempt, FailedAttempt, Failure}
import com.adamnfish.thorn.models.{Message, PlayerAddress, Serialisation}
import com.amazonaws.services.apigatewaymanagementapi.AmazonApiGatewayManagementApiAsync
import com.amazonaws.services.apigatewaymanagementapi.model.PostToConnectionRequest
import com.amazonaws.services.lambda.runtime.LambdaLogger

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal


class AwsMessaging(client: AmazonApiGatewayManagementApiAsync, logger: LambdaLogger) extends Messaging {
  override def sendMessage(playerAddress: PlayerAddress, message: Message)(implicit ec: ExecutionContext): Attempt[Unit] = {
    send(playerAddress, Serialisation.encodeMessage(message))
  }

  override def sendError(playerAddress: PlayerAddress, message: FailedAttempt)(implicit ec: ExecutionContext): Attempt[Unit] = {
    send(playerAddress, Serialisation.encodeFailure(message))
  }

  private def send(playerAddress: PlayerAddress, message: String)(implicit ec: ExecutionContext): Attempt[Unit] = {
    logger.log(s"Message (${playerAddress.address}): $message")
    val request = new PostToConnectionRequest()
      .withConnectionId(playerAddress.address)
      .withData(ByteBuffer.wrap(message.getBytes("UTF-8")))
    val fResult = AwsAsync.awsToScala(client.postToConnectionAsync)(request)
    Attempt.fromFuture(fResult) {
      case NonFatal(e) =>
        Failure(
          s"AWS messaging failure ${e.getMessage}",
          "Unable to send message to player",
          500,
          None,
          Some(e)
        ).asAttempt
    }.map(_ => ())
  }
}
