package com.adamnfish.skull.logic

import java.time.ZonedDateTime

import com.adamnfish.skull.models.{Game, GameId, Player}
import java.util.UUID.randomUUID


object Games {
  def newGame(creator: Player): Game = {
    val id = randomUUID().toString
    Game(
      gameId = GameId(id),
      players = List(creator),
      round = None,
      started = false,
      startTime = ZonedDateTime.now()
    )
  }
}
