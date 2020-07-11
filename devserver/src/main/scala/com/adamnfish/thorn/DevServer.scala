package com.adamnfish.thorn

import com.adamnfish.thorn.models.{Context, PlayerAddress}
import com.adamnfish.thorn.persistence.DynamoDB
import com.amazonaws.services.dynamodbv2.model.ScalarAttributeType._
import io.javalin.Javalin
import org.scanamo.LocalDynamoDB

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.io.AnsiColor
import scala.util.Random


object DevServer {
  val messaging = new DevMessaging
  val client = LocalDynamoDB.client()
  val db = new DynamoDB(client, "games", "players")
  LocalDynamoDB.createTable(client)("games")("gameCode" -> S, "gameId" -> S)
  LocalDynamoDB.createTable(client)("players")("gameId" -> S, "playerId" -> S)

  def main(args: Array[String]): Unit = {
    val app = Javalin.create()
    app.start(7000)

    app.ws("/api", { ws =>
      ws.onConnect { wctx =>
        val id = messaging.connect(wctx)
        println(s"Connected: ${displayId(id, fullId = true)}")
      }
      ws.onClose { wctx =>
        messaging.disconnect(wctx)
        println(s"Disconnected: ${displayId(wctx.getSessionId)}")
      }
      ws.onMessage { wctx =>
        // TODO: consider whether errors are sent as messages or responses?
        // (start with messages for simplicity, change to response in the future to save $$)
        println(s"Message: ${displayId(wctx.getSessionId)} <- ${wctx.message}")
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

  private def displayId(id: String, fullId: Boolean = false): String = {
    val seed = id.filterNot(_ == '-').sum
    val rng = new Random(seed)
    val colourCode = rng.shuffle(colours).head
    val idSummary =
        if (fullId) id
        else id.take(8)
    s"$colourCode$idSummary${AnsiColor.RESET}"
  }

  private val colours = List(
    s"${AnsiColor.RED_B}${AnsiColor.BOLD}${AnsiColor.BLACK}",
    s"${AnsiColor.RED_B}${AnsiColor.BOLD}${AnsiColor.GREEN}",
    s"${AnsiColor.RED_B}${AnsiColor.BOLD}${AnsiColor.YELLOW}",
    s"${AnsiColor.RED_B}${AnsiColor.BOLD}${AnsiColor.MAGENTA}",
    s"${AnsiColor.RED_B}${AnsiColor.BOLD}${AnsiColor.CYAN}",
    s"${AnsiColor.RED_B}${AnsiColor.BOLD}${AnsiColor.WHITE}",
    s"${AnsiColor.GREEN_B}${AnsiColor.BOLD}${AnsiColor.BLACK}",
    s"${AnsiColor.GREEN_B}${AnsiColor.BOLD}${AnsiColor.RED}",
    s"${AnsiColor.GREEN_B}${AnsiColor.BOLD}${AnsiColor.YELLOW}",
    s"${AnsiColor.GREEN_B}${AnsiColor.BOLD}${AnsiColor.BLUE}",
    s"${AnsiColor.GREEN_B}${AnsiColor.BOLD}${AnsiColor.MAGENTA}",
    s"${AnsiColor.GREEN_B}${AnsiColor.BOLD}${AnsiColor.WHITE}",
    s"${AnsiColor.YELLOW_B}${AnsiColor.BOLD}${AnsiColor.BLACK}",
    s"${AnsiColor.YELLOW_B}${AnsiColor.BOLD}${AnsiColor.RED}",
    s"${AnsiColor.YELLOW_B}${AnsiColor.BOLD}${AnsiColor.BLUE}",
    s"${AnsiColor.YELLOW_B}${AnsiColor.BOLD}${AnsiColor.MAGENTA}",
    s"${AnsiColor.YELLOW_B}${AnsiColor.BOLD}${AnsiColor.CYAN}",
    s"${AnsiColor.YELLOW_B}${AnsiColor.BOLD}${AnsiColor.WHITE}",
    s"${AnsiColor.BLUE_B}${AnsiColor.BOLD}${AnsiColor.BLACK}",
    s"${AnsiColor.BLUE_B}${AnsiColor.BOLD}${AnsiColor.RED}",
    s"${AnsiColor.BLUE_B}${AnsiColor.BOLD}${AnsiColor.YELLOW}",
    s"${AnsiColor.BLUE_B}${AnsiColor.BOLD}${AnsiColor.CYAN}",
    s"${AnsiColor.BLUE_B}${AnsiColor.BOLD}${AnsiColor.WHITE}",
    s"${AnsiColor.MAGENTA_B}${AnsiColor.BOLD}${AnsiColor.BLACK}",
    s"${AnsiColor.MAGENTA_B}${AnsiColor.BOLD}${AnsiColor.RED}",
    s"${AnsiColor.MAGENTA_B}${AnsiColor.BOLD}${AnsiColor.GREEN}",
    s"${AnsiColor.MAGENTA_B}${AnsiColor.BOLD}${AnsiColor.YELLOW}",
    s"${AnsiColor.MAGENTA_B}${AnsiColor.BOLD}${AnsiColor.CYAN}",
    s"${AnsiColor.MAGENTA_B}${AnsiColor.BOLD}${AnsiColor.WHITE}",
    s"${AnsiColor.CYAN_B}${AnsiColor.BOLD}${AnsiColor.BLACK}",
    s"${AnsiColor.CYAN_B}${AnsiColor.BOLD}${AnsiColor.RED}",
    s"${AnsiColor.CYAN_B}${AnsiColor.BOLD}${AnsiColor.YELLOW}",
    s"${AnsiColor.CYAN_B}${AnsiColor.BOLD}${AnsiColor.MAGENTA}",
    s"${AnsiColor.CYAN_B}${AnsiColor.BOLD}${AnsiColor.WHITE}",
  )
}
