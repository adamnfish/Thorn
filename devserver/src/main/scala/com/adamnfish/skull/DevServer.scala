package com.adamnfish.skull

import com.adamnfish.skull.models.{Context, PlayerAddress}
import io.javalin.Javalin
import org.scanamo.LocalDynamoDB

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global


object DevServer {
  val messaging = new DevMessaging
  val client = LocalDynamoDB.client()

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
        val result = Skull.main(wctx.message, context, messaging).tapErr { failure =>
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
