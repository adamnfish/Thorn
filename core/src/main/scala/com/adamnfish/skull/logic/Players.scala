package com.adamnfish.skull.logic

import com.adamnfish.skull.models.{Player, PlayerAddress, PlayerId, PlayerKey}
import java.util.UUID.randomUUID


object Players {
  def newPlayer(screenName: String, address: PlayerAddress): Player = {
    val id = randomUUID().toString
    val key = randomUUID().toString
    Player(
      screenName = screenName,
      playerId = PlayerId(id),
      playerKey = PlayerKey(key),
      playerAddress = address,
      0
    )
  }
}
