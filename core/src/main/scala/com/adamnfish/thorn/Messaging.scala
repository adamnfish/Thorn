package com.adamnfish.thorn

import com.adamnfish.thorn.attempt.{Attempt, FailedAttempt}
import com.adamnfish.thorn.models.{Message, PlayerAddress}

import scala.concurrent.ExecutionContext


trait Messaging {
  def sendMessage(playerAddress: PlayerAddress, message: Message)(implicit ec: ExecutionContext): Attempt[Unit]

  def sendError(playerAddress: PlayerAddress, message: FailedAttempt)(implicit ec: ExecutionContext): Attempt[Unit]
}
