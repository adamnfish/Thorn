package com.adamnfish.skull

import com.adamnfish.skull.models.{Context, PlayerAddress}
import com.amazonaws.services.dynamodbv2.model.ScalarAttributeType._
import io.javalin.Javalin
import org.scanamo.LocalDynamoDB

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


object DevServer {
  val messaging = new DevMessaging
  val client = LocalDynamoDB.client()
  LocalDynamoDB.createTable(client)("games")("gameId" -> S)
  LocalDynamoDB.createTable(client)("players")("gameId" -> S, "playerId" -> S)

  def main(args: Array[String]): Unit = {
    val app = Javalin.create()
    app.start(7000)

    app.ws("/api", { ws =>
      ws.onConnect { wctx =>
        val id = messaging.connect(wctx)
        println(s"id: $id")
      }
      ws.onClose { wctx =>
        messaging.disconnect(wctx)
      }
      ws.onMessage { wctx =>
        // TODO: consider whether errors are sent as messages or responses?
        // (start with messages for simplicity, change to response in the future to save $$)
        val context = Context(PlayerAddress(wctx.getSessionId))
        val result = Skull.main(wctx.message, context, messaging).tapFailure { failure =>
          println(s"[ERROR] ${failure.logString}")
        }
        Await.result(result.asFuture, 10.seconds)
      }
    })

    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      println("[INFO] Stopping...")
      app.stop()
    }))
  }
}
