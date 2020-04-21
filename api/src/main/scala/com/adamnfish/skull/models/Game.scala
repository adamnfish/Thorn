package com.adamnfish.skull.models

import java.time.ZonedDateTime


/*
 * Simplified representation for game logic.
 */

case class Game(
  gameId: String,
  players: List[Player],
  round: Option[Round],
  started: Boolean,
  startTime: ZonedDateTime
)

case class Player(
  screenName: String,
  playerId: PlayerId,
  playerKey: PlayerKey,
  playerAddress: PlayerAddress,
  score: Int,
)

trait Round
case class InitialDiscs(
  firstPlayer: PlayerId,
  initialDiscs: Map[PlayerId, Disc]
) extends Round
case class Placing(
  activePlayer: PlayerId,
  discs: Map[PlayerId, List[Disc]],
) extends Round
case class Bidding(
  activePlayer: PlayerId,
  discs: Map[PlayerId, List[Disc]],
  bids: Map[PlayerId, Int],
  passed: List[PlayerId],
) extends Round
case class Flipping(
  activePlayer: PlayerId,
  discs: Map[PlayerId, List[Disc]],
  revealed: Map[PlayerId, List[Disc]],
) extends Round
case class Finished(
  activePlayer: PlayerId,
  discs: Map[PlayerId, List[Disc]],
  revealed: Map[PlayerId, List[Disc]],
  successful: Boolean,
) extends Round


// wrappers to prevent accidental exposure of player keys
case class PlayerId(pid: String) extends AnyVal
case class PlayerKey(key: String) extends AnyVal
case class PlayerAddress(address: String) extends AnyVal
case class GameId(gid: String) extends AnyVal

// game elements

trait Disc
object Rose extends Disc
object Skull extends Disc
