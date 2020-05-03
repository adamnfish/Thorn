package com.adamnfish.skull.logic

import java.util.UUID.randomUUID

import com.adamnfish.skull.attempt.{Attempt, Failure}
import com.adamnfish.skull.models._


object Players {
  def newPlayer(screenName: String, address: PlayerAddress): Player = {
    val id = randomUUID().toString
    val key = randomUUID().toString
    Player(
      screenName = screenName,
      playerId = PlayerId(id),
      playerKey = PlayerKey(key),
      playerAddress = address,
      0
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

  def startPlayer(players: Map[PlayerId, Player]): Attempt[PlayerId] = {
    if (players.isEmpty) {
      Attempt.Left {
        Failure(
          "Can't choose start player from empty list",
          "Couldn't choose start player because there are no players",
          500
        ).asAttempt
      }
    } else {
      Attempt.Right {
        scala.util.Random.shuffle(players.keys.toList).head
      }
    }
  }
}
