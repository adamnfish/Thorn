package com.adamnfish.skull

import com.adamnfish.skull.attempt.Attempt
import com.adamnfish.skull.models.Message

import scala.concurrent.ExecutionContext


trait Messaging {
  def send(recipientId: String, message: Message)(implicit ec: ExecutionContext): Attempt[Unit]
}
