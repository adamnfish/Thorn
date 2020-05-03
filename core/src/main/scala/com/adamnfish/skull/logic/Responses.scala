package com.adamnfish.skull.logic

import com.adamnfish.skull.attempt.Attempt
import com.adamnfish.skull.models._

import scala.concurrent.ExecutionContext


object Responses {
  def tbd[A <: Message](): Response[A] = {
    ???
  }

  def empty: Response[Nothing] = {
    Response(None, Map.empty)
  }

  def ok(): Response[Status] = {
    Response(
      Some(Status("ok")),
      Map.empty
    )
  }

  def justRespond[A <: Message](msg: A): Response[A] = {
    Response(
      Some(msg),
      Map.empty
    )
  }

  def gameStatuses(game: Game)(implicit ec:ExecutionContext): Attempt[Response[GameStatus]] = {
    for {
      statuses <- Attempt.traverse(game.players.toList) { case (playerId, player) =>
        Representations.gameStatus(game, playerId).map(player.playerAddress -> _)
      }
    } yield {
      Response(
        None,
        statuses.toMap
      )
    }
  }
}
