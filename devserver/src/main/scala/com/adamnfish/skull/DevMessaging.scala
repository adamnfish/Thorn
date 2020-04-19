package com.adamnfish.skull

import java.util.UUID

import com.adamnfish.skull.attempt.{Attempt, Failure}
import com.adamnfish.skull.models.Message
import io.javalin.websocket.WsContext

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal


class DevMessaging extends Messaging {
  private val connections = new mutable.HashMap[String, WsContext]

  def connect(wctx: WsContext): String = {
    val id = UUID.randomUUID.toString
    connections.put(id, wctx)
    id
  }

  def disconnect(wctx: WsContext): Unit = {
    connections.find(_._2 == wctx).foreach { case (id, _) =>
      connections.remove(id)
    }
  }

  override def send(recipientId: String, message: Message)(implicit ec: ExecutionContext): Attempt[Unit] = {
    for {
      wctx <- Attempt.fromOption(connections.get(recipientId),
        Failure("User not connected", "Connection not found", 404).asAttempt
      )
      _ <-
        if (wctx.session.isOpen) {
          Attempt.unit
        } else {
          Attempt.Left {
            Failure("Connection has closed", "Connection closed", 400)
          }
        }
      result <-
        try {
          Attempt.Right {
            wctx.send("TODO: MESSAGE")
            ()
          }
        } catch {
          case NonFatal(e) =>
            Attempt.Left {
              Failure("Failed to send message", "Couldn't send message", 500, exception = Some(e))
            }
        }
    } yield result
  }
}
