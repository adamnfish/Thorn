package com.adamnfish.skull

import com.adamnfish.skull.attempt.{Attempt, FailedAttempt}
import com.adamnfish.skull.models.Message

import scala.concurrent.ExecutionContext


trait Messaging {
  def sendMessage(recipientId: String, message: Message)(ec: ExecutionContext): Attempt[Unit]

  def sendError(recipientId: String, message: FailedAttempt)(ec: ExecutionContext): Attempt[Unit]
}
