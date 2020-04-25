package com.adamnfish.skull.models


case class Context(
  playerAddress: PlayerAddress
)

case class Response[S <: Message](
  self: Option[S],
  messages: Map[PlayerAddress, GameStatus]
)
