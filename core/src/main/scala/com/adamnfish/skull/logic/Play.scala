package com.adamnfish.skull.logic

import com.adamnfish.skull.attempt.{Attempt, Failure}
import com.adamnfish.skull.models.{Bidding, Disc, Finished, Flipping, Game, InitialDiscs, Placing, Player, PlayerId, Round}

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

  private def allPlayersPlaced[A](discs: Map[PlayerId, List[Disc]], players: List[Player]): Boolean = {
    players.forall { player =>
      discs.getOrElse(player.playerId, Nil).nonEmpty
    }
  }

  def bidOnRound(count: Int, playerId: PlayerId, game: Game): Attempt[Game] = {
    val failure = (roundStr: String) => Attempt.Left(Failure(
      s"cannot bid in $roundStr round",
      "You can't place discs now",
      400
    ).asAttempt)
    game.round match {
      case None =>
        failure("none")
      case Some(_: InitialDiscs) =>
        failure("initial discs")
      case Some(_: Placing) =>
        failure("placing")
      case Some(bidding: Bidding) =>
        if (bidding.activePlayer != playerId) {
          Attempt.Left(
            Failure(
              "Cannot bid on another player's turn",
              "It's not your turn to bid",
              400
            )
          )
        } else {
          val max =
            if (bidding.bids.isEmpty) 0
            else bidding.bids.values.max
          if (count <= max) {
            Attempt.Left {
              Failure(
                "Bid not larger than previous bids",
                "Your bid must be larger than previous bids",
                400
              )
            }
          } else if (bidding.passed.contains(playerId)) {
            Attempt.Left {
              Failure(
                "Cannot bid after passing",
                "You can't bid after you have passed",
                400
              )
            }
          } else {
            Attempt.Right {
              game.copy(
                round = Some(
                  bidding.copy(
                    bids = bidding.bids.updated(playerId, count)
                  )
                )
              )
            }
          }
        }
      case Some(_: Flipping) =>
        failure("flipping")
      case Some(_: Finished) =>
        failure("finished")
    }
  }
}
