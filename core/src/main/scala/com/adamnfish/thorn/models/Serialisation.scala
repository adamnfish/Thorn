package com.adamnfish.thorn.models

import com.adamnfish.thorn.attempt.{Attempt, FailedAttempt, Failure}
import com.adamnfish.thorn.logic.Representations
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe._
import io.circe.syntax._
import io.circe.parser

import scala.concurrent.ExecutionContext


object Serialisation {
  def decodeRequest(jsonStr: String)(implicit ec: ExecutionContext): Attempt[Request] = {
    for {
      json <- parse(jsonStr, "Could not understand the request", None)
      request <- asAttempt(json, "The request isn't something I understand")(requestDecoder)
    } yield request
  }

  def encodeMessage(message: Message): String = {
    message.asJson.noSpaces
  }

  def encodeFailure(failures: FailedAttempt): String = {
    failures.asJson.noSpaces
  }


  private def parse(jsonStr: String, friendlyMessage: String, context: Option[String]): Attempt[Json] = {
    Attempt.fromEither {
      parser.parse(jsonStr).left.map { parsingFailure =>
        Failure(
          s"Failed to parse request body JSON: ${parsingFailure.message}",
          friendlyMessage,
          400,
          context,
          Some(parsingFailure)
        ).asAttempt
      }
    }
  }

  private def asAttempt[A](json: Json, friendlyMessage: String)(implicit decoder: Decoder[A]): Attempt[A] = {
    Attempt.fromEither {
      json.as[A].left.map { decodingFailure =>
        Failure(
          s"Failed to parse JSON as expected type: ${decodingFailure.message}",
          friendlyMessage,
          400,
          Some(decodingFailure.history.mkString("|")),
          Some(decodingFailure)
        ).asAttempt
      }
    }
  }

  // VALUE CLASSES
  private implicit val playerIdEncoder: Encoder[PlayerId] = Encoder.encodeString.contramap[PlayerId](_.pid)
  private implicit val playerKeyEncoder: Encoder[PlayerKey] = Encoder.encodeString.contramap[PlayerKey](_.key)
  private implicit val playerAddressEncoder: Encoder[PlayerAddress] = Encoder.encodeString.contramap[PlayerAddress](_.address)
  private implicit val gameIdEncoder: Encoder[GameId] = Encoder.encodeString.contramap[GameId](_.gid)

  private implicit val playerIdDecoder: Decoder[PlayerId] = Decoder.decodeString.emap[PlayerId](str => Right(PlayerId(str)))
  private implicit val playerKeyDecoder: Decoder[PlayerKey] = Decoder.decodeString.emap[PlayerKey](str => Right(PlayerKey(str)))
  private implicit val playerAddressDecoder: Decoder[PlayerAddress] = Decoder.decodeString.emap[PlayerAddress](str => Right(PlayerAddress(str)))
  private implicit val gameIdDecoder: Decoder[GameId] = Decoder.decodeString.emap[GameId](str => Right(GameId(str)))

  implicit val fooKeyEncoder: KeyEncoder[PlayerId] = new KeyEncoder[PlayerId] {
    override def apply(playerId: PlayerId): String = playerId.pid
  }

  // GAME TYPES
  private implicit val discEncoder: Encoder[Disc] = Encoder.encodeString.contramap[Disc] { disc =>
    Representations.discString(disc)
  }
  private implicit val discDecoder: Decoder[Disc] = Decoder.decodeString.emap[Disc] { discStr =>
    Representations.discFromString(discStr).left.map(_.friendlyMessage)
  }


  // SUMMARY TYPES
  private implicit val initialDiscsSummaryEncoder: Encoder[InitialDiscsSummary] = deriveEncoder[InitialDiscsSummary]
  private implicit val placingSummaryEncoder: Encoder[PlacingSummary] = deriveEncoder[PlacingSummary]
  private implicit val biddingSummaryEncoder: Encoder[BiddingSummary] = deriveEncoder[BiddingSummary]
  private implicit val flippingSummaryEncoder: Encoder[FlippingSummary] = deriveEncoder[FlippingSummary]
  private implicit val finishedSummaryEncoder: Encoder[FinishedSummary] = deriveEncoder[FinishedSummary]
  private implicit val roundSummaryEncoder: Encoder[RoundSummary] = Encoder.instance {
    case initialDiscsSummary: InitialDiscsSummary =>
      initialDiscsSummaryEncoder.apply(initialDiscsSummary)
    case placingSummary: PlacingSummary =>
      placingSummaryEncoder.apply(placingSummary)
    case biddingSummary: BiddingSummary =>
      biddingSummaryEncoder.apply(biddingSummary)
    case flippingSummary: FlippingSummary =>
      flippingSummaryEncoder.apply(flippingSummary)
    case finishedSummary: FinishedSummary =>
      finishedSummaryEncoder.apply(finishedSummary)
  }

  private implicit val playerSummaryEncoder: Encoder[PlayerSummary] = deriveEncoder[PlayerSummary]
  private implicit val selfSummaryEncoder: Encoder[SelfSummary] = deriveEncoder[SelfSummary]
  private implicit val gameSummaryEncoder: Encoder[GameSummary] = deriveEncoder[GameSummary]


  // REQUESTS
  private implicit val createGameDecoder: Decoder[CreateGame] = deriveDecoder[CreateGame]
  private implicit val joinGameDecoder: Decoder[JoinGame] = deriveDecoder[JoinGame]
  private implicit val startGameDecoder: Decoder[StartGame] = deriveDecoder[StartGame]
  private implicit val newRoundDecoder: Decoder[NewRound] = deriveDecoder[NewRound]
  private implicit val placeDiscDecoder: Decoder[PlaceDisc] = deriveDecoder[PlaceDisc]
  private implicit val bidDecoder: Decoder[Bid] = deriveDecoder[Bid]
  private implicit val passDecoder: Decoder[Pass] = deriveDecoder[Pass]
  private implicit val flipDecoder: Decoder[Flip] = deriveDecoder[Flip]
  private implicit val reconnectDecoder: Decoder[Reconnect] = deriveDecoder[Reconnect]
  private implicit val pingDecoder: Decoder[Ping] = deriveDecoder[Ping]
  private implicit val wakeDecoder: Decoder[Wake] = deriveDecoder[Wake]

  private val requestDecoder: Decoder[Request] = Decoder.instance(c =>
    c.downField("operation").as[String].flatMap {
      case "create-game" => c.as[CreateGame]
      case "join-game" => c.as[JoinGame]
      case "start-round" => c.as[StartGame]
      case "new-round" => c.as[NewRound]
      case "place-disc" => c.as[PlaceDisc]
      case "bid" => c.as[Bid]
      case "pass" => c.as[Pass]
      case "flip" => c.as[Flip]
      case "reconnect" => c.as[Reconnect]
      case "ping" => c.as[Ping]
      case "wake" => c.as[Wake]
    }
  )


  // MESSAGES
  private implicit val statusEncoder: Encoder[Status] = deriveEncoder[Status]
  private implicit val welcomeEncoder: Encoder[Welcome] = deriveEncoder[Welcome]
  private implicit val gameStatusEncoder: Encoder[GameStatus] = deriveEncoder[GameStatus]

  private implicit val messageEncoder: Encoder[Message] = Encoder.instance {
    case status: Status =>
      statusEncoder.apply(status)
    case welcome: Welcome =>
      welcomeEncoder.apply(welcome)
    case gameStatus: GameStatus =>
      gameStatusEncoder.apply(gameStatus)
  }


  // FAILURE
  private implicit val failureEncoder: Encoder[Failure] = Encoder.encodeJsonObject.contramap { failure =>
    val message =
      "friendlyMessage" -> Json.fromString(failure.friendlyMessage)
    val statusCode =
      "statusCode" -> Json.fromInt(failure.statusCode)
    failure.context match {
      case Some(context) =>
        JsonObject(
          message, statusCode, "context" -> Json.fromString(context)
        )
      case None =>
        JsonObject(message, statusCode)
    }
  }
  private implicit val failedAttemptEncoder: Encoder[FailedAttempt] = deriveEncoder[FailedAttempt]
}
