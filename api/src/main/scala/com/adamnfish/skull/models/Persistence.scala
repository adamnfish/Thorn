package com.adamnfish.skull.models

import java.time.ZonedDateTime


/*
 * Representation optimised for storage and updates.
 * Each class corresponds to a 'table'
 */

// Only updatable by a single player (creator or player on turn)
case class GameDB(
  gameId: String,  // PK
  playerIds: List[String],
  started: Boolean,
  startTime: ZonedDateTime,
  roundState: String,
  currentTurn: Option[String],
  revealedDiscs: Map[String, Int]
)

// only updatable by the player themselves
case class PlayerDB(
  playerId: PlayerId,  // PK
  gameId: GameId,      // SI
  playerKey: PlayerKey,
  playerAddress: PlayerAddress,
  screenName: String,
  successes: Int,
  discs: List[String],
  bid: Option[Int],
  passed: Option[Boolean],
)
