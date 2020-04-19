package com.adamnfish.skull.models


case class Context(
  playerAddress: PlayerAddress
)

case class Messages(
  messages: Map[PlayerAddress, Message]
)
object Messages {
  def empty = Messages(Map.empty)
}
