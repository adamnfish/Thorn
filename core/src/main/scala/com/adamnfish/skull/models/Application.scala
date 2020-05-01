package com.adamnfish.skull.models

import com.adamnfish.skull.Messaging
import com.adamnfish.skull.persistence.Database


case class Context(
  playerAddress: PlayerAddress,
  db: Database,
  messaging: Messaging,
)

case class Response[S <: Message](
  response: Option[S],
  messages: Map[PlayerAddress, GameStatus]
)
