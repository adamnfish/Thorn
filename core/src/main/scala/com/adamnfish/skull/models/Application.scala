package com.adamnfish.skull.models

import com.adamnfish.skull.persistence.Database


case class Context(
  playerAddress: PlayerAddress,
  db: Database,
)

case class Response[S <: Message](
  self: Option[S],
  messages: Map[PlayerAddress, GameStatus]
)
