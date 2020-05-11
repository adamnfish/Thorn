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
          case Some(round @ InitialDiscs(_, initialDiscs)) =>
            for {
              _ <-
                if (initialDiscs.getOrElse(playerId, Nil).isEmpty) Attempt.unit
                else Attempt.Left {
                  Failure(
                    "Cannot place second disc in initial discs round",
                    "You have already placed your disc",
                    400
                  )
                }
              newDiscs = initialDiscs.updatedWith(playerId) {
                case Some(Nil) =>
                  Some(List(disc))
                case None =>
                  Some(List(disc))
                case Some(current) =>
                  Some(current)
              }
            } yield {
              if (allPlayersPlaced(newDiscs, game.players))
                Placing(
                  activePlayer = round.firstPlayer,
                  discs = newDiscs,
                )
              else
                round.copy(initialDiscs = newDiscs)
            }
          case Some(round @ Placing(activePlayerId, discs)) =>
            if (activePlayerId == playerId) Attempt.Right {
              round.copy(
                discs = discs.updatedWith(playerId) {
                  case Some(current) =>
                    Some(disc :: current)
                  case None =>
                    Some(List(disc))
                }
              )
            } else Attempt.Left {
              Failure(
                "Cannot place disc on another player's turn",
                "You can't place a disc when it isn't your turn",
                400
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

  private def allPlayersPlaced[A](discs: Map[PlayerId, List[Disc]], players: Map[PlayerId, A]): Boolean = {
    players.keys.forall { playerId =>
      discs.getOrElse(playerId, Nil).nonEmpty
    }
  }
}
