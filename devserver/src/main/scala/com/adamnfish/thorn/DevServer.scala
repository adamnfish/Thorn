package com.adamnfish.thorn

import com.adamnfish.thorn.models.{Context, PlayerAddress}
import com.adamnfish.thorn.persistence.DynamoDB
import com.amazonaws.services.dynamodbv2.model.ScalarAttributeType._
import io.javalin.Javalin
import org.scanamo.LocalDynamoDB

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


object DevServer {
  val messaging = new DevMessaging
  val client = LocalDynamoDB.client()
  val db = new DynamoDB(client)
  LocalDynamoDB.createTable(client)("games")("gameCode" -> S, "gameId" -> S)
  LocalDynamoDB.createTable(client)("players")("gameId" -> S, "playerId" -> S)

  def main(args: Array[String]): Unit = {
    val app = Javalin.create()
    app.start(7000)

    app.ws("/api", { ws =>
      ws.onConnect { wctx =>
        val id = messaging.connect(wctx)
        println(s"Connected: $id")
      }
      ws.onClose { wctx =>
        messaging.disconnect(wctx)
        println(s"Disconnected: ${wctx.getSessionId}")
      }
      ws.onMessage { wctx =>
        // TODO: consider whether errors are sent as messages or responses?
        // (start with messages for simplicity, change to response in the future to save $$)
        println(s"Message: ${wctx.getSessionId} <- ${wctx.message}")
        val context = Context(PlayerAddress(wctx.getSessionId), db, messaging)
        val result = Thorn.main(wctx.message, context).tapFailure { failure =>
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
