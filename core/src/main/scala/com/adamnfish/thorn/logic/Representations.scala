package com.adamnfish.thorn.logic

import com.adamnfish.thorn.attempt.{Attempt, Failure}
import com.adamnfish.thorn.models._

import scala.concurrent.ExecutionContext


object Representations {
  // game state to persistence state

  def gameForDb(game: Game): GameDB = {
    GameDB(
      gameCode = Games.gameCode(game.gameId),
      gameId = game.gameId.gid,
      gameName = game.gameName,
      playerIds = game.players.map(_.playerId.pid),
      creatorId = game.creatorId.pid,
      started = game.started,
      startTime = game.startTime,
      roundState = roundKey(game.round),
      currentPlayer = game.round.map(currentPlayer),
      revealedDiscs = game.round.map(revealedDiscs).getOrElse(Map.empty),
    )
  }

  def newPlayerForDb(game: Game, player: Player): PlayerDB = {
    PlayerDB(
      gameId = game.gameId.gid,
      playerId = player.playerId.pid,
      playerKey = player.playerKey.key,
      playerAddress = player.playerAddress.address,
      screenName = player.screenName,
      score = player.score,
      placedDiscs = game.round.map(playerRoundDiscs(player.playerId)).getOrElse(Nil),
      roseCount = player.roseCount,
      hasThorn = player.hasThorn,
      bid = game.round.flatMap(playerRoundBid(player.playerId)).getOrElse(0),
      passed = game.round.flatMap(playerRoundPassed(player.playerId)).getOrElse(false),
    )
  }

  def playerForDb(game: Game, playerId: PlayerId)(implicit ec: ExecutionContext): Attempt[PlayerDB] = {
    Games.getPlayer(playerId, game)
      .map(newPlayerForDb(game, _))
  }

  def playersForDb(game: Game): List[PlayerDB] = {
    game.players
      .map(newPlayerForDb(game, _))
  }

  // unpack DB data

  def dbToGame(gameDB: GameDB, playerDBs: List[PlayerDB])(implicit ec: ExecutionContext): Attempt[Game] = {
    for {
      round <- gameDB.roundState match {
        case "none" =>
          Attempt.Right(None)
        case "initial-discs" =>
          for {
            pid <- Attempt.fromOption(gameDB.currentPlayer, Failure(
              "No current player for initial discs round",
              "Couldn't get start player for the game",
              500, None, None
            ).asAttempt)
            discs <- playerDiscs(playerDBs)
          } yield Some(InitialDiscs(
            firstPlayer = PlayerId(pid),
            initialDiscs = discs
          ))
        case "placing" =>
          for {
            pid <- Attempt.fromOption(gameDB.currentPlayer, Failure(
              "No current player for initial discs round",
              "Couldn't get start player for the game",
              500, None, None
            ).asAttempt)
            discs <- playerDiscs(playerDBs)
          } yield Some(Placing(
            activePlayer = PlayerId(pid),
            discs = discs
          ))
        case "bidding" =>
          for {
            pid <- Attempt.fromOption(gameDB.currentPlayer, Failure(
              "No current player for initial discs round",
              "Couldn't get start player for the game",
              500, None, None
            ).asAttempt)
            discs <- playerDiscs(playerDBs)
          } yield Some(Bidding(
            activePlayer = PlayerId(pid),
            discs = discs,
            bids = playerBids(playerDBs),
            passed = playerPasseds(playerDBs)
          ))
        case "flipping" =>
          for {
            pid <- Attempt.fromOption(gameDB.currentPlayer, Failure(
              "No current player for initial discs round",
              "Couldn't get start player for the game",
              500, None, None
            ).asAttempt)
            discs <- playerDiscs(playerDBs)
            revealed <- playerRevealeds(discs, gameDB.revealedDiscs)
            target <- Attempt.fromOption(
              playerBids(playerDBs).get(PlayerId(pid)),
              Failure(
                "Player cannot flip without a bid",
                "The active player hasn't made a bid yet",
                500
              ).asAttempt
            )
          } yield Some(Flipping(
            activePlayer = PlayerId(pid),
            target = target,
            discs = discs,
            revealed = revealed,
          ))
        case "finished" =>
          for {
            pid <- Attempt.fromOption(gameDB.currentPlayer, Failure(
              "No current player for initial discs round",
              "Couldn't get start player for the game",
              500, None, None
            ).asAttempt)
            discs <- playerDiscs(playerDBs)
            revealed <- playerRevealeds(discs, gameDB.revealedDiscs)
          } yield Some(Finished(
            activePlayer = PlayerId(pid),
            discs = discs,
            revealed = revealed,
            successful = roundIsSuccessful(revealed)
          ))
        case _ =>
          Attempt.Left(Failure(
            s"Invalid roundState: ${gameDB.roundState}",
            "Couldn't load game round",
            500,
            Some("roundState"),
            None
          ))
      }
      playerOrderingError = Failure(
        "Database players out of sync with game players",
        "There was an error loading this game's players",
        500
      )
      orderedPlayerIds <-
        if (playerDBs.map(_.playerId).toSet == gameDB.playerIds.toSet) {
          Attempt.Right(gameDB.playerIds)
        } else {
          Attempt.Left(playerOrderingError)
        }
      allPlayers = playerDBs.map(dbToPlayer)
      orderedPlayers <- Attempt.fromOption(
        sortByKeyList(orderedPlayerIds, allPlayers)(_.playerId.pid),
        playerOrderingError.asAttempt
      )
    } yield {
      Game(
        gameId = GameId(gameDB.gameId),
        gameName = gameDB.gameName,
        creatorId = PlayerId(gameDB.creatorId),
        players = orderedPlayers,
        round = round,
        started = gameDB.started,
        startTime = gameDB.startTime,
      )
    }
  }

  private def dbToPlayer(playerDB: PlayerDB): Player = {
    Player(
      screenName = playerDB.screenName,
      playerId = PlayerId(playerDB.playerId),
      playerKey = PlayerKey(playerDB.playerKey),
      playerAddress = PlayerAddress(playerDB.playerAddress),
      score = playerDB.score,
      roseCount = playerDB.roseCount,
      hasThorn = playerDB.hasThorn,
    )
  }

  // create transport representation

  def gameStatus(game: Game, playerId: PlayerId)(implicit ec: ExecutionContext): Attempt[GameStatus] = {
    for {
      player <- Attempt.fromOption(game.players.find(_.playerId == playerId),
        Failure(
          "Couldn't summarise game for invalid playerId",
          "Couldn't create game summary",
          500
        ).asAttempt
      )
    } yield {
      val playerSummaries = game.players.map { player =>
        PlayerSummary(
          screenName = player.screenName,
          playerId = player.playerId,
          score = player.score
        )
      }
      val round = game.round.map {
        case InitialDiscs(firstPlayer, initialDiscs) =>
          InitialDiscsSummary(firstPlayer, toDiscCount(initialDiscs))
        case Placing(activePlayer, discs) =>
          PlacingSummary(activePlayer, toDiscCount(discs))
        case Bidding(activePlayer, discs, bids, passed) =>
          BiddingSummary(activePlayer, toDiscCount(discs), bids, passed)
        case Flipping(activePlayer, target, discs, revealed) =>
          FlippingSummary(activePlayer, target, toDiscCount(discs), revealed)
        case Finished(activePlayer, discs, revealed, successful) =>
          FinishedSummary(activePlayer, toDiscCount(discs), revealed, successful)
      }
      // TODO: edge case here where we return None for player discs instead of
      //  failing, if we're unable to extract info from the round
      val playerDiscs = game.round.flatMap {
        case InitialDiscs(_, initialDiscs) =>
          initialDiscs.get(playerId)
        case Placing(_, discs) =>
          discs.get(playerId)
        case Bidding(_, discs, _, _) =>
          discs.get(playerId)
        case Flipping(_, _, discs, _) =>
          discs.get(playerId)
        case Finished(_, discs, _, _) =>
          discs.get(playerId)
      }

      GameStatus(
        SelfSummary(
          screenName = player.screenName,
          playerId = player.playerId,
          score = player.score,
          placedDiscs = playerDiscs,
          roseCount = player.roseCount,
          hasThorn = player.hasThorn,
        ),
        GameSummary(
          gameId = game.gameId,
          gameName = game.gameName,
          creatorId = game.creatorId,
          players = playerSummaries,
          round = round,
        )
      )
    }
  }

  // Helper fns

  def discString(disc: Disc): String = {
    disc match {
      case Thorn =>
        "thorn"
      case Rose =>
        "rose"
    }
  }

  def discFromString(discStr: String): Either[Failure, Disc] = {
    discStr match {
      case "thorn" =>
        Right(Thorn)
      case "rose" =>
        Right(Rose)
      case _ =>
        Left(
          Failure(
            s"Could not parse Disc repr, $discStr",
            s"Invalid disc $discStr, expected `thorn` or `rose`",
            500,
            None
          )
        )
    }
  }

  private def roundKey(roundOpt: Option[Round]): String = {
    roundOpt match {
      case None =>
        "none"
      case Some(round) =>
        round match {
          case _: InitialDiscs =>
            "initial-discs"
          case _: Placing =>
            "placing"
          case _: Bidding =>
            "bidding"
          case _: Flipping =>
            "flipping"
          case _: Finished =>
            "finished"
        }
    }
  }

  private def currentPlayer(round: Round): String = {
    round match {
      case InitialDiscs(firstPlayer, _) =>
        firstPlayer.pid
      case Placing(activePlayer,_) =>
        activePlayer.pid
      case Bidding(activePlayer,_, _, _) =>
        activePlayer.pid
      case Flipping(activePlayer, _, _, _) =>
        activePlayer.pid
      case Finished(activePlayer, _, _, _) =>
        activePlayer.pid
    }
  }

  private def revealedDiscs(round: Round): Map[String, Int] = {
    round match {
      case InitialDiscs(_, _) =>
        Map.empty
      case Placing(_, _) =>
        Map.empty
      case Bidding(_, _, _, _) =>
        Map.empty
      case Flipping(_, _, _, revealed) =>
        revealed.map { case (playerId, discs) =>
          playerId.pid -> discs.length
        }
      case Finished(_, _, revealed, _) =>
        revealed.map { case (playerId, discs) =>
          playerId.pid -> discs.length
        }
    }
  }

  private def playerRoundDiscs(playerId: PlayerId)(round: Round): List[String] = {
    round match {
      case InitialDiscs(_, initialDiscs) =>
        initialDiscs.getOrElse(playerId, Nil).map(discString)
      case Placing(_, discs) =>
        discs.getOrElse(playerId, Nil).map(discString)
      case Bidding(_, discs, _, _) =>
        discs.getOrElse(playerId, Nil).map(discString)
      case Flipping(_, _, discs, _) =>
        discs.getOrElse(playerId, Nil).map(discString)
      case Finished(_, discs, _, _) =>
        discs.getOrElse(playerId, Nil).map(discString)
    }
  }

  private def playerRoundBid(playerId: PlayerId)(round: Round): Option[Int] = {
    round match {
      case Bidding(_, _, bids, _) =>
        bids.get(playerId)
      case _ =>
        None
    }
  }

  private def playerRoundPassed(playerId: PlayerId)(round: Round): Option[Boolean] = {
    round match {
      case Bidding(_, _, _, passed) =>
        Some(passed.contains(playerId))
      case _ =>
        None
    }
  }

  private def playerDiscs(playerDBs: List[PlayerDB])(implicit ec: ExecutionContext): Attempt[Map[PlayerId, List[Disc]]] = {
    Attempt.traverse(playerDBs) { playerDB =>
      Attempt.traverse(playerDB.placedDiscs) { discStr =>
        Attempt.fromEither(
          discFromString(discStr)
            .left.map(_.asAttempt)
        )
      }.map(PlayerId(playerDB.playerId) -> _)
    }.map(_.toMap)
  }

  private def playerRevealeds(discs: Map[PlayerId, List[Disc]], revealedCounts: Map[String, Int])(implicit ec: ExecutionContext): Attempt[Map[PlayerId, List[Disc]]] = {
    Attempt.traverse(discs.toList) { case (playerId, discs) =>
      val count = revealedCounts.getOrElse(playerId.pid, 0)
      if (count > discs.length) {
        Attempt.Left {
          Failure(
            "More discs revealed than are available",
            "Too many discs have been revealed",
            500
          )
        }
      } else {
        Attempt.Right(
          playerId -> discs.take(count)
        )
      }
    }.map(_.toMap)
  }

  private def playerBids(playerDBs: List[PlayerDB])(implicit ec: ExecutionContext): Map[PlayerId, Int] = {
    playerDBs.map { playerDB =>
      PlayerId(playerDB.playerId) -> playerDB.bid
    }.toMap
  }

  private def playerPasseds(playerDBs: List[PlayerDB])(implicit ec: ExecutionContext): List[PlayerId] = {
    playerDBs
      .filter(_.passed)
      .map(p => PlayerId(p.playerId))
  }

  private def roundIsSuccessful(revealeds: Map[PlayerId, List[Disc]]): Boolean = {
    !revealeds.values.flatten.exists(_ == Thorn)
  }

  private def toDiscCount(discs: Map[PlayerId, List[Disc]]): Map[PlayerId, Int] = {
    discs.view.mapValues(_.length).toMap
  }

  private[logic] def sortByKeyList[A, K](keyOrder: List[K], as: List[A])(keyFn: A => K): Option[List[A]] = {
    val asMap = as.map(a => keyFn(a) -> a).toMap
    if (asMap.keys.toSet == keyOrder.toSet) {
      keyOrder.foldRight[Option[List[A]]](Some(Nil)) {
        case (k, Some(acc)) =>
          asMap.get(k).map(_ :: acc)
        case (_, None) =>
          None
      }
    } else {
      None
    }
  }
}
