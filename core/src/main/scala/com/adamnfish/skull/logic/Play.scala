package com.adamnfish.skull.logic

import com.adamnfish.skull.attempt.{Attempt, Failure}
import com.adamnfish.skull.models.{Bidding, Disc, Finished, Flipping, Game, InitialDiscs, Placing, PlayerId, Round}

import scala.concurrent.ExecutionContext


object Play {
  def nextStartPlayer(finished: Finished): PlayerId = {
    ???
  }

  def placeDisc(disc: Disc, playerId: PlayerId, game: Game)(implicit ec: ExecutionContext): Attempt[Game] = {
    for {
      newRound <- {
        val failure = (roundStr: String) => Failure(
          s"cannot place discs in $roundStr round",
          "You can't place discs now",
          400
        ).asAttempt
        game.round match {
          case None =>
            Attempt.Left {
              failure("none")
            }
          case Some(round@InitialDiscs(_, initialDiscs)) =>
            Attempt.Right {
              round.copy(
                initialDiscs = initialDiscs.updatedWith(playerId) {
                  case Some(current) =>
                    Some(disc :: current)
                  case None =>
                    Some(List(disc))
                }
              )
            }
          case Some(round@Placing(_, discs)) =>
            Attempt.Right {
              round.copy(
                discs = discs.updatedWith(playerId) {
                  case Some(current) =>
                    Some(disc :: current)
                  case None =>
                    Some(List(disc))
                }
              )
            }
          case Some(_: Bidding) =>
            Attempt.Left {
              failure("bidding")
            }
          case Some(_: Flipping) =>
            Attempt.Left {
              failure("flipping")
            }
          case Some(_: Finished) =>
            Attempt.Left {
              failure("finished")
            }
        }
      }
    } yield game.copy(round = Some(newRound))
  }
}
