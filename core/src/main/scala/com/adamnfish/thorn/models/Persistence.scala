package com.adamnfish.thorn.models

import java.time.ZonedDateTime


/*
 * Representation optimised for storage and updates.
 * Each class corresponds to a 'table'
 */

// Only updatable by a single player (creator or player on turn)
case class GameDB(
  gameCode: String,  // Partition
  gameId: String,    // Sort
  gameName: String,
  creatorId: String,
  playerIds: List[String],
  started: Boolean,
  startTime: ZonedDateTime,
  roundState: String,
  currentPlayer: Option[String], // player id
  revealedDiscs: Map[String, Int]
)

// only updatable by the player themselves
case class PlayerDB(
  gameId: String,    // Partition
  playerId: String,  // Sort
  playerKey: String,
  playerAddress: String,
  screenName: String,
  score: Int,
  placedDiscs: List[String],
  roseCount: Int,
  hasThorn: Boolean,
  bid: Int,
  passed: Boolean,
)
