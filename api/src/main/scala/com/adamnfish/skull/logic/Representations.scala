package com.adamnfish.skull.logic

import com.adamnfish.skull.models.{Game, GameDB, PlayerDB, PlayerId, GameStatus}


object Representations {
  // game state to persistence state
  def gameForDb(game: Game): GameDB = ???
  def playerForDb(game: Game, playerId: PlayerId): PlayerDB = ???

  // unpack DB data
  def dbToGame(gameDB: GameDB, playerDBs: List[PlayerDB]): Game = ???

  // create transport representation
  def status(game: Game, playerId: PlayerId): GameStatus = ???
}
