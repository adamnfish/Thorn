package com.adamnfish.skull.logic

import com.adamnfish.skull.attempt.{Attempt, Failure}
import com.adamnfish.skull.models.{Bidding, BiddingSummary, Disc, Finished, FinishedSummary, Flipping, FlippingSummary, Game, GameDB, GameId, GameStatus, GameSummary, InitialDiscs, InitialDiscsSummary, Placing, PlacingSummary, Player, PlayerAddress, PlayerDB, PlayerId, PlayerKey, PlayerSummary, Rose, Round, SelfSummary, Skull}

import scala.concurrent.ExecutionContext


object Representations {
  // game state to persistence state

  def gameForDb(game: Game): GameDB = {
    GameDB(
      gameId = game.gameId.gid,
      gameName = game.gameName,
      playerIds = game.players.values.map(_.playerId.pid).toList,
      started = game.started,
      startTime = game.startTime,
      roundState = roundKey(game.round),
      currentPlayer = game.round.map(currentPlayer),
      revealedDiscs = game.round.map(revealedDiscs).getOrElse(Map.empty),
    )
  }

  def playerForDb(game: Game, player: Player): PlayerDB = {
    PlayerDB(
      gameId = game.gameId.gid,
      playerId = player.playerId.pid,
      playerKey = player.playerKey.key,
      playerAddress = player.playerAddress.address,
      screenName = player.screenName,
      score = player.score,
      discs = game.round.map(playerRoundDiscs(player.playerId)).getOrElse(Nil),
      bid = game.round.flatMap(playerRoundBid(player.playerId)),
      passed = game.round.flatMap(playerRoundPassed(player.playerId)),
    )
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
          } yield Some(Flipping(
            activePlayer = PlayerId(pid),
            discs = discs,
            revealed = revealed
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
    } yield {
      Game(
        gameId = GameId(gameDB.gameId),
        gameName = gameDB.gameName,
        players = playerDBs.map(dbToPlayer).toMap,
        round = round,
        started = gameDB.started,
        startTime = gameDB.startTime,
      )
    }
  }

  private def dbToPlayer(playerDB: PlayerDB): (PlayerId, Player) = {
    val playerId = PlayerId(playerDB.playerId)
    playerId -> Player(
      screenName = playerDB.screenName,
      playerId = playerId,
      playerKey = PlayerKey(playerDB.playerKey),
      playerAddress = PlayerAddress(playerDB.playerAddress),
      playerDB.score
    )
  }

  // create transport representation

  def gameStatus(game: Game, playerId: PlayerId)(implicit ec: ExecutionContext): Attempt[GameStatus] = {
    for {
      player <- Attempt.fromOption(game.players.get(playerId),
        Failure(
          "Couldn't summarise game for invalid playerId",
          "Couldn't create game summary",
          500
        ).asAttempt
      )
    } yield {
      val playerSummaries = game.players.map { case (_, player) =>
        PlayerSummary(
          screenName = player.screenName,
          playerId = player.playerId,
          score = player.score
        )
      }.toList
      val round = game.round.map {
        case InitialDiscs(firstPlayer, initialDiscs) =>
          InitialDiscsSummary(firstPlayer, toDiscCount(initialDiscs))
        case Placing(activePlayer, discs) =>
          PlacingSummary(activePlayer, toDiscCount(discs))
        case Bidding(activePlayer, discs, bids, passed) =>
          BiddingSummary(activePlayer, toDiscCount(discs), bids, passed)
        case Flipping(activePlayer, discs, revealed) =>
          FlippingSummary(activePlayer, toDiscCount(discs), revealed)
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
        case Flipping(_, discs, _) =>
          discs.get(playerId)
        case Finished(_, discs, _, _) =>
          discs.get(playerId)
      }

      GameStatus(
        SelfSummary(
          screenName = player.screenName,
          playerId = player.playerId,
          score = player.score,
          discs = playerDiscs,
        ),
        GameSummary(
          gameId = game.gameId,
          gameName = game.gameName,
          players = playerSummaries,
          round = round,
        )
      )
    }
  }

  // Helper fns

  def discString(disc: Disc): String = {
    disc match {
      case Skull =>
        "skull"
      case Rose =>
        "rose"
    }
  }

  def discFromString(discStr: String): Either[Failure, Disc] = {
    discStr match {
      case "skull" =>
        Right(Skull)
      case "rose" =>
        Right(Rose)
      case _ =>
        Left(
          Failure(
            s"Could not parse Disc repr, $discStr",
            s"Invalid disc $discStr, expected `skull` or `rose`",
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
      case Flipping(activePlayer, _, _) =>
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
      case Flipping(_, _, revealed) =>
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
      case Flipping(_, discs, _) =>
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
      Attempt.traverse(playerDB.discs) { discStr =>
        Attempt.fromEither(
          discFromString(discStr)
            .left.map(_.asAttempt)
        )
      }.map(PlayerId(playerDB.playerId) -> _)
    }.map(_.toMap)
  }

  private def playerRevealeds(discs: Map[PlayerId, List[Disc]], revealedCounts: Map[String, Int])(implicit ec: ExecutionContext): Attempt[Map[PlayerId, List[Disc]]] = {
    Attempt.traverse(discs.toList) { case (playerId, discs) =>
      revealedCounts.get(playerId.pid).map { count =>
        Attempt.Right(
          playerId -> discs.take(count)
        )
      }.getOrElse {
        Attempt.Left(
          Failure(
            s"No revealed count for player ${playerId.pid}",
            "Couldn't see how many discs each player has revealed",
            500, None, None
          )
        )
      }
    }.map(_.toMap)
  }

  private def playerBids(playerDBs: List[PlayerDB])(implicit ec: ExecutionContext): Map[PlayerId, Int] = {
    playerDBs.map { playerDB =>
      PlayerId(playerDB.playerId) -> playerDB.bid.getOrElse(0)
    }.toMap
  }

  private def playerPasseds(playerDBs: List[PlayerDB])(implicit ec: ExecutionContext): List[PlayerId] = {
    playerDBs
      .filter(_.passed.contains(true))
      .map(p => PlayerId(p.playerId))
  }

  private def roundIsSuccessful(revealeds: Map[PlayerId, List[Disc]]): Boolean = {
    !revealeds.values.flatten.exists(_ == Skull)
  }

  private def toDiscCount(discs: Map[PlayerId, List[Disc]]): Map[PlayerId, Int] = {
    discs.view.mapValues(_.length).toMap
  }
}
