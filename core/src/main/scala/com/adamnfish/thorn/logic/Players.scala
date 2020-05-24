package com.adamnfish.thorn.logic

import java.util.UUID.randomUUID

import com.adamnfish.thorn.attempt.{Attempt, Failure}
import com.adamnfish.thorn.models._

import scala.util.Random


object Players {
  def newPlayer(screenName: String, address: PlayerAddress): Player = {
    val id = randomUUID().toString
    val key = randomUUID().toString
    Player(
      screenName = screenName,
      playerId = PlayerId(id),
      playerKey = PlayerKey(key),
      playerAddress = address,
      score = 0,
      roseCount = 3,
      hasThorn = true,
    )
  }

  def ensureAllPlayersPresent(gameDB: GameDB, playerDBs: List[PlayerDB]): Attempt[Unit] = {
    gameDB.playerIds.filterNot(pid => playerDBs.exists(_.playerId == pid)) match {
      case Nil =>
        Attempt.unit
      case missingIds =>
        Attempt.Left {
          Failure(
            s"Game players result is missing players `${missingIds.mkString(",")}`",
            "Couldn't fetch all the players for this game",
            500
          )
        }
    }
  }

  def removeDiscFromThisPlayer(playerId: PlayerId, players: List[Player]): Attempt[List[Player]] = {
    if (players.exists(_.playerId == playerId)) {
      Attempt.Right {
        players.map { player =>
          if (player.playerId == playerId) removePlayerDisc(player)
          else player
        }
      }
    } else {
      Attempt.Left {
        Failure(
          "Cannot remove a disc from a player that isn't in this game",
          "Unable to discard a disc from a player that cannot be found",
          500
        )
      }
    }
  }

  private[logic] def removePlayerDisc(player: Player): Player = {
    if (player.hasThorn && 0 == Random.between(0, 1 + player.roseCount)) {
      player.copy(hasThorn = false)
    } else {
      player.copy(roseCount = math.max(0, player.roseCount - 1))
    }
  }

  def outOfDiscs(player: Player): Boolean = {
    discCount(player) <= 0
  }

  def discCount(player: Player): Int = {
    if (player.hasThorn) 1 + player.roseCount
    else player.roseCount
  }
}
