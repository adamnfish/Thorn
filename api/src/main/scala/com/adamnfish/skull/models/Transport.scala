package com.adamnfish.skull.models


/*
 * Secrets-free versions of the game models for transmission
 */

case class GameSummary(
  gameId: GameId,
  players: List[PlayerSummary],
  round: Option[RoundSummary],
)

case class PlayerSummary(
  screenName: String,
  playerId: PlayerId,
  score: Int,
)

case class SelfSummary(
  screenName: String,
  playerId: PlayerId,
  score: Int,
  discs: Option[List[Disc]]
)

trait RoundSummary
case class InitialDiscsSummary(
  initialDiscs: Map[PlayerId, Disc]
) extends RoundSummary
case class PlacingSummary(
  discs: Map[PlayerId, Int],
  turn: PlayerId,
) extends RoundSummary
case class BiddingSummary(
  passed: List[PlayerId],
  discs: Map[PlayerId, Int],
  bids: Map[PlayerId, Int],
  turn: PlayerId,
) extends RoundSummary
case class FlippingSummary(
  player: PlayerId,
  discs: Map[PlayerId, Int],
  revealed: Map[PlayerId, List[Disc]],
) extends RoundSummary
case class FinishedSummary(
  player: PlayerId,
  successful: Boolean,
  discs: Map[PlayerId, Int],
  revealed: Map[PlayerId, List[Disc]],
) extends RoundSummary

// requests

sealed trait Request
case class CreateGame(
  screenName: String,
  gameName: String,
) extends Request
case class JoinGame(
  gameId: GameId,
  screenName: String,
) extends Request
case class NewRound(
  gameId: GameId,
  playerId: PlayerId,
) extends Request
case class StartRound(
  gameId: GameId,
  playerId: PlayerId,
) extends Request
case class PlaceDisc(
  gameId: GameId,
  playerId: PlayerId,
  disc: Disc,
) extends Request
case class Bid(
  gameId: GameId,
  playerId: PlayerId,
  count: Int,
) extends Request
case class Pass(
  gameId: GameId,
  playerId: PlayerId,
) extends Request
case class Flip(
  gameId: GameId,
  playerId: PlayerId,
  stack: PlayerId,
) extends Request
case class Ping(
  gameId: GameId,
  playerId: PlayerId,
) extends Request
case class Wake(
) extends Request


// messages (data sent to clients)
sealed trait Message
case class Status(
  message: String
) extends Message
case class Welcome(
  playerKey: PlayerKey,
  playerId: PlayerId,
  gameId: GameId,
)
case class GameStatus(
  self: SelfSummary,
  game: GameSummary,
) extends Message
