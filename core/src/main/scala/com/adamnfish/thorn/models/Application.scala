package com.adamnfish.thorn.models

import com.adamnfish.thorn.Messaging
import com.adamnfish.thorn.persistence.Database


case class Context(
  playerAddress: PlayerAddress,
  db: Database,
  messaging: Messaging,
)

case class Response[S <: Message](
  response: Option[S],
  messages: Map[PlayerAddress, GameStatus]
)
