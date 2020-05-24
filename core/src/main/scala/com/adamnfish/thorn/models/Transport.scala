package com.adamnfish.thorn.models

/*
 * Secrets-free versions of the game models for transmission
 */

case class GameSummary(
  gameId: GameId,
  gameName: String,
  creatorId: PlayerId,
  players: List[PlayerSummary],
  round: Option[RoundSummary],
)

case class PlayerSummary(
  screenName: String,
  playerId: PlayerId,
  score: Int,
  discCount: Int,
)

case class SelfSummary(
  screenName: String,
  playerId: PlayerId,
  score: Int,
  placedDiscs: Option[List[Disc]],
  roseCount: Int,
  hasThorn: Boolean,
)

sealed trait RoundSummary extends Product
case class InitialDiscsSummary(
  activePlayer: PlayerId,
  initialDiscs: Map[PlayerId, Int]
) extends RoundSummary
case class PlacingSummary(
  activePlayer: PlayerId,
  discs: Map[PlayerId, Int],
) extends RoundSummary
case class BiddingSummary(
  activePlayer: PlayerId,
  discs: Map[PlayerId, Int],
  bids: Map[PlayerId, Int],
  passed: List[PlayerId],
) extends RoundSummary
case class FlippingSummary(
  activePlayer: PlayerId,
  target: Int,
  discs: Map[PlayerId, Int],
  revealed: Map[PlayerId, List[Disc]],
) extends RoundSummary
case class FinishedSummary(
  activePlayer: PlayerId,
  discs: Map[PlayerId, Int],
  revealed: Map[PlayerId, List[Disc]],
  successful: Boolean,
) extends RoundSummary

// requests

sealed trait Request
case class CreateGame(
  screenName: String,
  gameName: String,
) extends Request
case class JoinGame(
  gameCode: String,
  screenName: String,
) extends Request
case class StartGame(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
  playerOrder: List[PlayerId],
) extends Request
case class PlaceDisc(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
  disc: Disc,
) extends Request
case class Bid(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
  count: Int,
) extends Request
case class Pass(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
) extends Request
case class Flip(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
  stack: PlayerId,
) extends Request
case class NewRound(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
) extends Request
case class Reconnect(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
) extends Request
case class Ping(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
) extends Request
case class Wake(
) extends Request


// messages (data sent to clients)
sealed trait Message extends Product
case class Status(
  message: String
) extends Message
case class Welcome(
  playerKey: PlayerKey,
  playerId: PlayerId,
  gameId: GameId,
) extends Message
case class GameStatus(
  self: SelfSummary,
  game: GameSummary,
) extends Message
