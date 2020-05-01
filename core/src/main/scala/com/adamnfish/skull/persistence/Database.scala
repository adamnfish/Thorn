package com.adamnfish.skull.persistence

import com.adamnfish.skull.attempt.Attempt
import com.adamnfish.skull.models.{GameDB, GameId, PlayerDB}

import scala.concurrent.ExecutionContext


trait Database {
  def getGame(gameId: GameId)(implicit ec:ExecutionContext): Attempt[Option[GameDB]]

  def getPlayers(gameId: GameId)(implicit ec:ExecutionContext): Attempt[List[PlayerDB]]

  def writeGame(gameDB: GameDB)(implicit ec:ExecutionContext): Attempt[Unit]

  def writePlayer(playerDB: PlayerDB)(implicit ec:ExecutionContext): Attempt[Unit]
}
