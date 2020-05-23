package com.adamnfish.thorn.logic

import java.time.ZonedDateTime

import com.adamnfish.thorn.models.{Bidding, BiddingSummary, Disc, Finished, FinishedSummary, Flipping, FlippingSummary, Game, GameDB, GameId, InitialDiscs, InitialDiscsSummary, Placing, PlacingSummary, Player, PlayerAddress, PlayerDB, PlayerId, PlayerKey, Rose, Thorn}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import Representations._
import com.adamnfish.thorn.{AttemptValues, TestHelpers}
import org.scalacheck.Arbitrary._
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class RepresentationsTest
  extends AnyFreeSpec with Matchers with OptionValues with AttemptValues
    with TestHelpers with ScalaCheckDrivenPropertyChecks {
  val now = ZonedDateTime.now()
  val p1id = PlayerId("id-1")
  val game = Game(
    GameId("id"),
    "game name",
    p1id,
    Nil,
    None,
    started = false,
    startTime = now
  )
  val player1 = Player(
    "Sreen name 1", PlayerId("id-1"), PlayerKey("key-1"), PlayerAddress("address-1"), 0, 3, true
  )
  val player2 = Player(
    "Sreen name 2", PlayerId("id-2"), PlayerKey("key-2"), PlayerAddress("address-2"), 0, 3, true
  )

  "gameForDb" - {
    "uses game's game id" in {
      forAll { (id: String) =>
        gameForDb(
          game.copy(gameId = GameId(id))
        ).gameId shouldEqual id
      }
    }

    "uses game's game id to generate the game code" in {
      forAll { (id: String) =>
        whenever(id.length >=4) {
          val gameCode = gameForDb(
            game.copy(gameId = GameId(id))
          ).gameCode
          id should startWith(gameCode)
        }
      }
    }

    "uses game's game name" in {
      forAll { (gn: String) =>
        gameForDb(
          game.copy(gameName = gn)
        ).gameName shouldEqual gn
      }
    }


    "uses game's creator id" in {
      forAll { (creatorId: String) =>
        gameForDb(
          game.copy(creatorId = PlayerId(creatorId))
        ).creatorId shouldEqual creatorId
      }
    }

    "uses game's started value" in {
      forAll { (s: Boolean) =>
        gameForDb(
          game.copy(started = s)
        ).started shouldEqual s
      }
    }

    "uses game's start time" in {
      gameForDb(game).startTime shouldEqual game.startTime
    }

    "playerIds" - {
      "is empty if game has no players" in {
        gameForDb(
          game.copy(players = Nil)
        ).playerIds shouldBe empty
      }

      "contains player id for single player" in {
        gameForDb(
          game.copy(players = List(player1))
        ).playerIds shouldEqual List("id-1")
      }

      "contains player ids for multiple players" in {
        gameForDb(
          game.copy(players = List(
            player1, player2,
          ))
        ).playerIds shouldEqual List("id-1", "id-2")
      }
    }

    "correctly unpacks round information" - {
      "roundState" - {
        "for no round" - {
          val noRoundGame = game.copy(round = None)

          "gameState is 'none'" in {
            gameForDb(noRoundGame).roundState shouldEqual "none"
          }

          "current player is empty" in {
            gameForDb(noRoundGame).currentPlayer shouldEqual None
          }

          "revealed discs is empty" in {
            gameForDb(noRoundGame).revealedDiscs shouldEqual Map.empty
          }
        }

        "for InitialDiscs" - {
          val round = InitialDiscs(player1.playerId, Map.empty)

          "gameState is 'initial-discs'" in {
            gameForDb(
              game.copy(round = Some(round))
            ).roundState shouldEqual "initial-discs"
          }

          "current player's id comes from round's 'firstPlayer'" in {
            gameForDb(
              game.copy(round = Some(round))
            ).currentPlayer shouldEqual Some(player1.playerId.pid)
          }

          "revealed discs is empty" in {
            gameForDb(
              game.copy(round = Some(
                round.copy(initialDiscs = Map(
                  player1.playerId -> List(Thorn),
                  player2.playerId -> List(Rose),
                ))
              ))
            ).revealedDiscs shouldEqual Map.empty
          }
        }

        "for Placing" - {
          val round = Placing(player1.playerId, Map.empty)

          "gameState is 'placing'" in {
            gameForDb(
              game.copy(round = Some(round))
            ).roundState shouldEqual "placing"
          }

          "current player's id comes from round's 'activePlayer'" in {
            gameForDb(
              game.copy(round = Some(round))
            ).currentPlayer shouldEqual Some(player1.playerId.pid)
          }

          "revealed discs is empty" in {
            gameForDb(
              game.copy(round = Some(
                round.copy(discs = Map(
                  player1.playerId -> List(Rose, Thorn),
                  player2.playerId -> List(Rose),
                ))
              ))
            ).revealedDiscs shouldEqual Map.empty
          }
        }

        "for Bidding" - {
          val round = Bidding(player1.playerId, Map.empty, Map.empty, Nil)

          "gameState is 'bidding'" in {
            gameForDb(
              game.copy(round = Some(round))
            ).roundState shouldEqual "bidding"
          }

          "current player's id comes from round's 'activePlayer'" in {
            gameForDb(
              game.copy(round = Some(round))
            ).currentPlayer shouldEqual Some(player1.playerId.pid)
          }

          "revealed discs is empty" in {
            gameForDb(
              game.copy(round = Some(
                round.copy(discs = Map(
                  player1.playerId -> List(Rose, Thorn),
                  player2.playerId -> List(Rose),
                ))
              ))
            ).revealedDiscs shouldEqual Map.empty
          }
        }

        "for Flipping" - {
          val round = Flipping(player1.playerId, 2, Map.empty, Map.empty)
          val roundWithDiscs = round.copy(
            discs = Map(
              player1.playerId -> List(Rose, Thorn),
              player2.playerId -> List(Rose),
            )
          )

          "gameState is 'flipping'" in {
            gameForDb(
              game.copy(round = Some(round))
            ).roundState shouldEqual "flipping"
          }

          "current player's id comes from round's 'activePlayer'" in {
            gameForDb(
              game.copy(round = Some(round))
            ).currentPlayer shouldEqual Some(player1.playerId.pid)
          }

          "revealed discs is empty if round has no revealed discs" in {
            gameForDb(
              game.copy(round = Some(
                roundWithDiscs.copy(revealed = Map.empty)
              ))
            ).revealedDiscs shouldEqual Map.empty
          }

          "revealed disc counts matches round's partially revealed discs" in {
            gameForDb(
              game.copy(round = Some(
                roundWithDiscs.copy(
                  revealed = Map(
                    player2.playerId -> List(Rose)
                  )
                )
              ))
            ).revealedDiscs shouldEqual Map(
              player2.playerId.pid -> 1
            )
          }

          "revealed disc counts matches round's revealed discs" in {
            gameForDb(
              game.copy(round = Some(
                roundWithDiscs.copy(
                  revealed = Map(
                    player1.playerId -> List(Thorn, Rose),
                    player2.playerId -> List(Rose),
                  )
                )
              ))
            ).revealedDiscs shouldEqual Map(
              player1.playerId.pid -> 2,
              player2.playerId.pid -> 1,
            )
          }
        }

        "for Finished" - {
          val round = Finished(player1.playerId, Map.empty, Map.empty, false)
          val roundWithDiscs = round.copy(
            discs = Map(
              player1.playerId -> List(Rose, Thorn),
              player2.playerId -> List(Rose),
            )
          )

          "gameState is 'finished'" in {
            gameForDb(
              game.copy(round = Some(round))
            ).roundState shouldEqual "finished"
          }

          "current player's id comes from round's 'activePlayer'" in {
            gameForDb(
              game.copy(round = Some(round))
            ).currentPlayer shouldEqual Some(player1.playerId.pid)
          }

          "revealed disc counts matches round's revealed discs" in {
            gameForDb(
              game.copy(round = Some(
                roundWithDiscs.copy(
                  revealed = Map(
                    player1.playerId -> List(Thorn, Rose),
                    player2.playerId -> List(Rose),
                  )
                )
              ))
            ).revealedDiscs shouldEqual Map(
              player1.playerId.pid -> 2,
              player2.playerId.pid -> 1,
            )
          }
        }
      }
    }
  }

  "newPlayerForDb" - {
    "takes game id from the provided game" in {
      forAll { (gameId: String) =>
        newPlayerForDb(
          game.copy(gameId = GameId(gameId)),
          player1
        ).gameId shouldEqual gameId
      }
    }

    "takes player id from the provided player" in {
      forAll { (playerId: String) =>
        newPlayerForDb(
          game,
          player1.copy(playerId = PlayerId(playerId))
        ).playerId shouldEqual playerId
      }
    }

    "takes player key from the provided player" in {
      forAll { (playerKey: String) =>
        newPlayerForDb(
          game,
          player1.copy(playerKey = PlayerKey(playerKey))
        ).playerKey shouldEqual playerKey
      }
    }

    "takes player address from the provided player" in {
      forAll { (playerAddress: String) =>
        newPlayerForDb(
          game,
          player1.copy(playerAddress = PlayerAddress(playerAddress))
        ).playerAddress shouldEqual playerAddress
      }
    }

    "takes screen name from the provided player" in {
      forAll { (screenName: String) =>
        newPlayerForDb(
          game,
          player1.copy(screenName = screenName)
        ).screenName shouldEqual screenName
      }
    }

    "takes score from the provided player" in {
      forAll { (score: Int) =>
        newPlayerForDb(
          game,
          player1.copy(score = score)
        ).score shouldEqual score
      }
    }

    "correctly unpacks round information" - {
      "when round is empty" - {
        "player's bid is empty" in {
          newPlayerForDb(game, player1).bid shouldEqual 0
        }

        "passed is empty" in {
          newPlayerForDb(game, player1).passed shouldEqual false
        }

        "discs is empty" in {
          newPlayerForDb(game, player1).placedDiscs shouldEqual Nil
        }
      }

      "when round is InitialDiscs" - {
        val round = InitialDiscs(player1.playerId, Map.empty)
        val gameRound = game.copy(round = Some(round))

        "player's bid is empty" in {
          newPlayerForDb(gameRound, player1).bid shouldEqual 0
        }

        "passed is empty" in {
          newPlayerForDb(gameRound, player1).passed shouldEqual false
        }

        "discs is empty for an empty round" in {
          newPlayerForDb(gameRound, player1).placedDiscs shouldEqual Nil
        }

        "discs is empty when only other players have placed" in {
          newPlayerForDb(
            game.copy(
              round = Some(round.copy(initialDiscs = Map(player2.playerId -> List(Rose))))
            ),
            player1
          ).placedDiscs shouldEqual Nil
        }

        "discs is populated from this player's initial discs" in {
          newPlayerForDb(
            game.copy(
              round = Some(round.copy(
                initialDiscs = Map(
                  player1.playerId -> List(Thorn),
                  player2.playerId -> List(Rose),
                )
              ))
            ),
            player1
          ).placedDiscs shouldEqual List("thorn")
        }
      }

      "when round is Placing" - {
        val round = Placing(player1.playerId, Map.empty)
        val gameRound = game.copy(round = Some(round))

        "player's bid is empty" in {
          newPlayerForDb(gameRound, player1).bid shouldEqual 0
        }

        "passed is empty" in {
          newPlayerForDb(gameRound, player1).passed shouldEqual false
        }

        "discs is populated from this player's discs" in {
          newPlayerForDb(
            game.copy(
              round = Some(round.copy(
                discs = Map(
                  player1.playerId -> List(Rose, Thorn),
                  player2.playerId -> List(Rose),
                )
              ))
            ),
            player1
          ).placedDiscs shouldEqual List("rose", "thorn")
        }
      }

      "when round is Bidding" - {
        val round = Bidding(player1.playerId, Map.empty, Map.empty, Nil)

        "player's bid is empty if no bid information exists for them" in {
          newPlayerForDb(
            game.copy(
              round = Some(round.copy(
                bids = Map(player2.playerId -> 1)
              ))
            ),
            player1
          ).bid shouldEqual 0
        }

        "player's bid comes from round information, if present" in {
          forAll { (bid: Int) =>
            newPlayerForDb(
              game.copy(
                round = Some(round.copy(
                  bids = Map(player1.playerId -> bid)
                ))
              ),
              player1
            ).bid shouldEqual bid
          }
        }

        "passed is set to player's pass status in the round" in {
          forAll { (passed: Boolean) =>
            newPlayerForDb(
              game.copy(
                round = Some(round.copy(
                  passed =
                    if (passed) List(player1.playerId)
                    else Nil
                ))
              ),
              player1
            ).passed shouldEqual passed
          }
        }

        "discs is populated from this player's discs" in {
          newPlayerForDb(
            game.copy(
              round = Some(round.copy(
                discs = Map(
                  player1.playerId -> List(Rose, Thorn),
                  player2.playerId -> List(Rose),
                )
              ))
            ),
            player1
          ).placedDiscs shouldEqual List("rose", "thorn")
        }
      }

      "when round is Flipping" - {
        val round = Flipping(player1.playerId, 3, Map.empty, Map.empty)
        val gameRound = game.copy(round = Some(round))

        "player's bid is empty" in {
          newPlayerForDb(gameRound, player1).bid shouldEqual 0
        }

        "passed is empty" in {
          newPlayerForDb(gameRound, player1).passed shouldEqual false
        }

        "discs is populated from this player's discs" in {
          newPlayerForDb(
            game.copy(
              round = Some(round.copy(
                discs = Map(
                  player1.playerId -> List(Rose, Thorn),
                  player2.playerId -> List(Rose),
                )
              ))
            ),
            player1
          ).placedDiscs shouldEqual List("rose", "thorn")
        }
      }

      "when round is Finished" - {
        val round = Finished(player1.playerId, Map.empty, Map.empty, false)
        val gameRound = game.copy(round = Some(round))

        "player's bid is empty" in {
          newPlayerForDb(gameRound, player1).bid shouldEqual 0
        }

        "passed is empty" in {
          newPlayerForDb(gameRound, player1).passed shouldEqual false
        }

        "discs is populated from this player's discs" in {
          newPlayerForDb(
            game.copy(
              round = Some(round.copy(
                discs = Map(
                  player1.playerId -> List(Rose, Thorn),
                  player2.playerId -> List(Rose),
                )
              ))
            ),
            player1
          ).placedDiscs shouldEqual List("rose", "thorn")
        }
      }
    }
  }

  "newPlayer" - {
    "returns playerDb if player exists in the game" in {
      playerForDb(
        game.copy(
          players = List(
            player1, player2,
          )
        ),
        player1.playerId
      ).value() shouldEqual newPlayerForDb(game, player1)
    }

    "if player does not exist in the game, fails" in {
      playerForDb(
        game.copy(
          players = List(
            player1, player2,
          )
        ),
        PlayerId("another player id")
      ).isFailedAttempt()
    }
  }

  "dbToGame" - {
    val p1id = PlayerId("id-1")
    val p2id = PlayerId("id-2")
    val player1 = PlayerDB(
      "game-id", p1id.pid, "key-1", "address-1", "Sreen name 1", 0,
      Nil, 3, true, 0, false
    )
    val player2 = PlayerDB(
      "game-id", p2id.pid, "key-2", "address-2", "Sreen name 2", 0,
      Nil, 3, true, 0, false
    )
    val playerDBs = List(player1, player2)
    val gameDb = GameDB(
      gameCode = "code",
      gameId = "game-id",
      gameName = "game name",
      creatorId = p1id.pid,
      playerIds = List(player1.playerId, player2.playerId),
      started = false,
      startTime = now,
      roundState = "none",
      currentPlayer = None,
      revealedDiscs = Map.empty
    )

    "uses gameDB's game ID" in {
      forAll { (gameId: String) =>
        dbToGame(
          gameDb.copy(gameId = gameId),
          playerDBs
        ).value().gameId.gid shouldEqual gameId
      }
    }

    "uses gameDB's game name" in {
      forAll { (gameName: String) =>
        dbToGame(
          gameDb.copy(gameName = gameName),
          playerDBs
        ).value().gameName shouldEqual gameName
      }
    }

    "uses gameDB's started value" in {
      forAll { (started: Boolean) =>
        dbToGame(
          gameDb.copy(started = started),
          playerDBs
        ).value().started shouldEqual started
      }
    }

    "uses gameDB's creator id" in {
      forAll { (creatorId: String) =>
        dbToGame(
          gameDb.copy(creatorId = creatorId),
          playerDBs
        ).value().creatorId.pid shouldEqual creatorId
      }
    }

    "uses gameDB's start time" in {
      dbToGame(gameDb, playerDBs).value().startTime shouldEqual gameDb.startTime
    }

    "generates round correctly" - {
      "if roundState is 'none'" - {
        val gameDbWithRound = gameDb.copy(roundState = "none")

        "round is None" in {
          dbToGame(gameDbWithRound, playerDBs).value().round shouldEqual None
        }
      }

      "if roundState is initial-discs" - {
        val gameDBWithRound = gameDb.copy(
          roundState = "initial-discs",
          currentPlayer = Some(p1id.pid),
        )
        val playersDBsWithDiscs = List(
          player1.copy(placedDiscs = List("thorn")),
          player2.copy(placedDiscs = List("rose")),
        )

        "round is an instance of InitialDiscs" in {
          val result = dbToGame(gameDBWithRound, playersDBsWithDiscs).value()
          result.round.value shouldBe a[InitialDiscs]
        }

        "InitialDiscs is correctly populated" in {
          val result = dbToGame(gameDBWithRound, playersDBsWithDiscs).value()
          result.round.value.asInstanceOf[InitialDiscs] should have(
            "firstPlayer" as p1id.pid,
            "initialDiscs" as Map(
              p1id -> List(Thorn),
              p2id -> List(Rose),
            ),
          )
        }
      }

      "if roundState is placing" - {
        val gameDBWithRound = gameDb.copy(
          roundState = "placing",
          currentPlayer = Some(p1id.pid),
        )
        val playersDBsWithDiscs = List(
          player1.copy(placedDiscs = List("thorn")),
          player2.copy(placedDiscs = List("rose", "rose")),
        )

        "round is an instance of Placing" in {
          val result = dbToGame(gameDBWithRound, playersDBsWithDiscs).value()
          result.round.value shouldBe a[Placing]
        }

        "Placing is correctly populated" in {
          val result = dbToGame(gameDBWithRound, playersDBsWithDiscs).value()
          result.round.value.asInstanceOf[Placing] should have(
            "activePlayer" as p1id.pid,
            "discs" as Map(
              p1id -> List(Thorn),
              p2id -> List(Rose, Rose),
            ),
          )
        }
      }

      "if roundState is bidding" - {
        val gameDBWithRound = gameDb.copy(
          roundState = "bidding",
          currentPlayer = Some(p1id.pid),
        )
        val playersDBsWithDiscs = List(
          player1.copy(
            placedDiscs = List("thorn", "rose"),
            passed = false,
            bid = 2
          ),
          player2.copy(
            placedDiscs = List("rose", "rose"),
            passed = true,
            bid = 0
          ),
        )

        "round is an instance of Bidding" in {
          val result = dbToGame(gameDBWithRound, playersDBsWithDiscs).value()
          result.round.value shouldBe a[Bidding]
        }

        "Bidding is correctly populated" in {
          val result = dbToGame(gameDBWithRound, playersDBsWithDiscs).value()
          result.round.value.asInstanceOf[Bidding] should have(
            "activePlayer" as p1id.pid,
            "discs" as Map(
              p1id -> List(Thorn, Rose),
              p2id -> List(Rose, Rose),
            ),
            "bids" as Map(
              p1id -> 2,
              p2id -> 0,
            ),
            "passed" as List(
              p2id
            )
          )
        }
      }

      "if roundState is flipping" - {
        val gameDBWithRound = gameDb.copy(
          roundState = "flipping",
          currentPlayer = Some(p1id.pid),
          revealedDiscs = Map(
            p1id.pid -> 1,
            p2id.pid -> 2,
          )
        )
        val playersDBsWithDiscs = List(
          player1.copy(
            placedDiscs = List("thorn", "rose"),
            passed = false,
            bid = 2
          ),
          player2.copy(
            placedDiscs = List("rose", "rose"),
            passed = true,
            bid = 0
          ),
        )

        "round is an instance of Flipping" in {
          val result = dbToGame(gameDBWithRound, playersDBsWithDiscs).value()
          result.round.value shouldBe a[Flipping]
        }

        "Flipping is correctly populated" in {
          val result = dbToGame(gameDBWithRound, playersDBsWithDiscs).value()
          result.round.value.asInstanceOf[Flipping] should have(
            "activePlayer" as p1id.pid,
            "discs" as Map(
              p1id -> List(Thorn, Rose),
              p2id -> List(Rose, Rose),
            ),
            "revealed" as Map(
              p1id -> List(Thorn),
              p2id -> List(Rose, Rose),
            ),
          )
        }
      }

      "if roundState is finished" - {
        val gameDBWithRound = gameDb.copy(
          roundState = "finished",
          currentPlayer = Some(p1id.pid),
          revealedDiscs = Map(
            p1id.pid -> 1,
            p2id.pid -> 2,
          )
        )
        val playersDBsWithDiscs = List(
          player1.copy(
            placedDiscs = List("thorn", "rose"),
            passed = false,
            bid = 2
          ),
          player2.copy(
            placedDiscs = List("rose", "rose"),
            passed = true,
            bid = 0
          ),
        )

        "round is an instance of Finished" in {
          val result = dbToGame(gameDBWithRound, playerDBs).value()
          result.round.value shouldBe a[Finished]
        }

        "Finished is correctly populated" in {
          val result = dbToGame(gameDBWithRound, playersDBsWithDiscs).value()
          result.round.value.asInstanceOf[Finished] should have(
            "activePlayer" as p1id.pid,
            "successful" as false,
            "discs" as Map(
              p1id -> List(Thorn, Rose),
              p2id -> List(Rose, Rose),
            ),
            "revealed" as Map(
              p1id -> List(Thorn),
              p2id -> List(Rose, Rose),
            ),
          )
        }
      }
    }

    "generates players correctly" - {
      "uses player IDs from the provided playerDBs" in {
        val result = dbToGame(gameDb, playerDBs).value()
        result.players.map(_.playerId).toSet shouldEqual Set(
          p1id, p2id,
        )
      }

      "uses the order from gameDB's player IDs" in {
        def test(playerOrder: List[String]) = {
          val result = dbToGame(
            gameDb.copy(
              playerIds = playerOrder
            ),
            playerDBs
          ).value()
          result.players.map(_.playerId.pid) shouldEqual playerOrder
        }

        test(List(player1.playerId, player2.playerId))
        test(List(player2.playerId, player1.playerId))
      }

      "example player 1 is correctly populated" in {
        val result = dbToGame(gameDb, playerDBs).value()
        result.players.find(_.playerId == p1id).value should have(
          "screenName" as player1.screenName,
          "playerId" as p1id.pid,
          "playerKey" as player1.playerKey,
          "playerAddress" as player1.playerAddress,
          "score" as 0,
        )
      }

      "example player 2 is correctly populated" in {
        val result = dbToGame(gameDb, playerDBs).value()
        result.players.find(_.playerId == p2id).value should have(
          "screenName" as player2.screenName,
          "playerId" as p2id.pid,
          "playerKey" as player2.playerKey,
          "playerAddress" as player2.playerAddress,
          "score" as 0,
        )
      }
    }
  }

  "gameStatus" - {
    "fails if the provided playerId is not found" in {
      gameStatus(
        game.copy(
          players = List(player1)
        ),
        PlayerId("INVALID PLAYER ID")
      ).isFailedAttempt()
    }

    "sets GameStatus correctly" - {
      "sets gameId from game" in {
        forAll { (gameId: String) =>
          val gid = GameId(gameId)
          gameStatus(
            game.copy(
              gameId = gid,
              players = List(player1)
            ),
            player1.playerId
          ).value().game.gameId shouldEqual gid
        }
      }

      "sets gameName from game" in {
        forAll { (gameName: String) =>
          gameStatus(
            game.copy(
              gameName = gameName,
              players = List(player1)
            ),
            player1.playerId
          ).value().game.gameName shouldEqual gameName
        }
      }

      "sets creator id from game" in {
        forAll { (creatorId: String) =>
          gameStatus(
            game.copy(
              creatorId = PlayerId(creatorId),
              players = List(player1)
            ),
            player1.playerId
          ).value().game.creatorId.pid shouldEqual creatorId
        }
      }

      "player summaries are set correctly" - {
        val gameWithPlayers = game.copy(
          players = List(
            player1, player2,
          )
        )

        "player summaries exist for each player" in {
          gameStatus(
            gameWithPlayers, player1.playerId
          ).value().game.players.map(_.playerId).toSet shouldEqual Set(
            player1.playerId, player2.playerId
          )
        }

        "player 1's summary is set correctly" in {
          val playerSummary = gameStatus(
            gameWithPlayers, player1.playerId
          ).value().game.players.find(_.playerId == player1.playerId).value
          playerSummary should have(
            "screenName" as player1.screenName,
            "playerId" as player1.playerId.pid,
            "score" as player1.score,
          )
        }

        "player 2's summary is set correctly" in {
          val playerSummary = gameStatus(
            gameWithPlayers, player2.playerId
          ).value().game.players.find(_.playerId == player2.playerId).value
          playerSummary should have(
            "screenName" as player2.screenName,
            "playerId" as player2.playerId.pid,
            "score" as player2.score,
          )
        }
      }

      "round summary is set correctly" - {
        "if round is no round" in {
          gameStatus(
            game.copy(
              players = List(
                player1, player2,
              ),
              round = None
            ),
            player1.playerId
          ).value().game.round shouldEqual None
        }

        "if round is initial discs" - {
          val gameWithRound = game.copy(
            players = List(
              player1, player2,
            ),
            round = Some(InitialDiscs(
              firstPlayer = player1.playerId,
              initialDiscs = Map(
                player1.playerId -> List(Rose),
                player2.playerId -> Nil,
              )
            ))
          )

          "is an instance of InitialDiscsSummary" in {
            gameStatus(
              gameWithRound,
              player1.playerId
            ).value().game.round.value shouldBe a[InitialDiscsSummary]
          }

          "is correctly populated" in {
            gameStatus(
              gameWithRound,
              player1.playerId
            ).value().game.round.value should have(
              "activePlayer" as player1.playerId.pid,
              "initialDiscs" as Map(
                player1.playerId -> 1,
                player2.playerId -> 0,
              ),
            )
          }
        }

        "if round is placing" - {
          val gameWithRound = game.copy(
            players = List(
              player1, player2,
            ),
            round = Some(Placing(
              activePlayer = player1.playerId,
              discs = Map(
                player1.playerId -> List(Rose),
                player2.playerId -> List(Thorn, Rose),
              )
            ))
          )

          "is an instance of PlacingSummary" in {
            gameStatus(
              gameWithRound,
              player1.playerId
            ).value().game.round.value shouldBe a[PlacingSummary]
          }

          "is correctly populated" in {
            gameStatus(
              gameWithRound,
              player1.playerId
            ).value().game.round.value should have(
              "activePlayer" as player1.playerId.pid,
              "discs" as Map(
                player1.playerId -> 1,
                player2.playerId -> 2,
              ),
            )
          }
        }

        "if round is bidding" - {
          val gameWithRound = game.copy(
            players = List(
              player1, player2,
            ),
            round = Some(Bidding(
              activePlayer = player1.playerId,
              discs = Map(
                player1.playerId -> List(Rose),
                player2.playerId -> List(Thorn, Rose),
              ),
              bids = Map(
                player1.playerId -> 2,
                player2.playerId -> 1,
              ),
              passed = List(player2.playerId)
            ))
          )

          "is instance of BiddingSummary" in {
            gameStatus(
              gameWithRound,
              player1.playerId
            ).value().game.round.value shouldBe a[BiddingSummary]
          }

          "is correctly populated" in {
            gameStatus(
              gameWithRound,
              player1.playerId
            ).value().game.round.value should have(
              "activePlayer" as player1.playerId.pid,
              "discs" as Map(
                player1.playerId -> 1,
                player2.playerId -> 2,
              ),
              "bids" as Map(
                player1.playerId -> 2,
                player2.playerId -> 1,
              ),
              "passed" as List(
                player2.playerId
              ),
            )
          }
        }

        "if round is flipping" - {
          val gameWithRound = game.copy(
            players = List(
              player1, player2,
            ),
            round = Some(Flipping(
              activePlayer = player1.playerId,
              target = 2,
              discs = Map(
                player1.playerId -> List(Rose),
                player2.playerId -> List(Thorn, Rose),
              ),
              revealed = Map(
                player1.playerId -> List(Rose),
                player2.playerId -> List(Thorn),
              ),
            ))
          )

          "is instance of FlippingSummary" in {
            gameStatus(
              gameWithRound,
              player1.playerId
            ).value().game.round.value shouldBe a[FlippingSummary]
          }

          "is correctly populated" in {
            gameStatus(
              gameWithRound,
              player1.playerId
            ).value().game.round.value should have(
              "activePlayer" as player1.playerId.pid,
              "discs" as Map(
                player1.playerId -> 1,
                player2.playerId -> 2,
              ),
              "revealed" as Map(
                player1.playerId -> List(Rose),
                player2.playerId -> List(Thorn),
              ),
            )
          }
        }

        "if round is finished" - {
          val gameWithRound = game.copy(
            players = List(
              player1, player2,
            ),
            round = Some(Finished(
              activePlayer = player1.playerId,
              discs = Map(
                player1.playerId -> List(Rose),
                player2.playerId -> List(Thorn, Rose),
              ),
              revealed = Map(
                player1.playerId -> List(Rose),
                player2.playerId -> List(Thorn),
              ),
              successful = false,
            ))
          )

          "is instance of FinishedSummary" in {
            gameStatus(
              gameWithRound,
              player1.playerId
            ).value().game.round.value shouldBe a[FinishedSummary]
          }

          "is correctly populated" in {
            gameStatus(
              gameWithRound,
              player1.playerId
            ).value().game.round.value should have(
              "activePlayer" as player1.playerId.pid,
              "discs" as Map(
                player1.playerId -> 1,
                player2.playerId -> 2,
              ),
              "revealed" as Map(
                player1.playerId -> List(Rose),
                player2.playerId -> List(Thorn),
              ),
              "successful" as false,
            )
          }
        }
      }
    }

    "sets SelfSummary correctly" - {
      "screen name is taken from player's screen name" in {
        forAll { (screenName: String) =>
          gameStatus(
            game.copy(
              players = List(
                player1.copy(
                  screenName = screenName
                )
              )
            ),
            player1.playerId
          ).value().self.screenName shouldEqual screenName
        }
      }

      "player id is taken from player's id" in {
        forAll { (pid: String) =>
          gameStatus(
            game.copy(
              players = List(
                player1.copy(
                  playerId = PlayerId(pid)
                )
              )
            ),
            PlayerId(pid),
          ).value().self.playerId.pid shouldEqual pid
        }
      }

      "score is taken from player's score" in {
        forAll { (score: Int) =>
          gameStatus(
            game.copy(
              players = List(
                player1.copy(
                  score = score
                )
              )
            ),
            player1.playerId,
          ).value().self.score shouldEqual score
        }
      }

      "discs is populated correctly for round" - {
        "if there is no round, discs is None" in {
          gameStatus(
            game.copy(
              players = List(player1),
              round = None
            ),
            player1.playerId,
          ).value().self.placedDiscs shouldEqual None
        }

        "InitialDiscs discs are drawn from round discs" in {
          forAll { (discs: List[Disc]) =>
            gameStatus(
              game.copy(
                players = List(player1),
                round = Some(InitialDiscs(
                  player1.playerId,
                  Map(
                    player1.playerId -> discs
                  )
                ))
              ),
              player1.playerId,
            ).value().self.placedDiscs.value shouldEqual discs
          }
        }

        "Placing discs are drawn from round discs" in {
          forAll { (discs: List[Disc]) =>
            gameStatus(
              game.copy(
                players = List(player1),
                round = Some(Placing(
                  player1.playerId,
                  Map(
                    player1.playerId -> discs
                  )
                ))
              ),
              player1.playerId,
            ).value().self.placedDiscs.value shouldEqual discs
          }
        }

        "Bidding discs are drawn from round discs" in {
          forAll { (discs: List[Disc]) =>
            gameStatus(
              game.copy(
                players = List(player1),
                round = Some(Bidding(
                  player1.playerId,
                  Map(
                    player1.playerId -> discs
                  ),
                  Map.empty,
                  Nil,
                ))
              ),
              player1.playerId,
            ).value().self.placedDiscs.value shouldEqual discs
          }
        }

        "Flipping discs are drawn from round discs" in {
          forAll { (discs: List[Disc]) =>
            gameStatus(
              game.copy(
                players = List(player1),
                round = Some(Flipping(
                  player1.playerId,
                  target = 3,
                  Map(
                    player1.playerId -> discs
                  ),
                  Map.empty,
                ))
              ),
              player1.playerId,
            ).value().self.placedDiscs.value shouldEqual discs
          }
        }

        "Finished discs are drawn from round discs" in {
          forAll { (discs: List[Disc]) =>
            gameStatus(
              game.copy(
                players = List(player1),
                round = Some(Finished(
                  player1.playerId,
                  Map(
                    player1.playerId -> discs
                  ),
                  Map.empty,
                  false
                ))
              ),
              player1.playerId,
            ).value().self.placedDiscs.value shouldEqual discs
          }
        }
      }
    }
  }

  "sortByKeyList" - {
    "sorts by the key" in {
      val result = sortByKeyList(
        List(2, 1, 3),
        List((1, "123"), (2, "234"), (3, "345"))
      )(_._1).value
      result shouldEqual List((2, "234"), (1, "123"), (3, "345"))
    }

    "returns None if there is a missing key" in {
      val result = sortByKeyList(
        List(2, 1),
        List((1, "123"), (2, "234"), (3, "345"))
      )(_._1)
      result shouldEqual None
    }

    "returns None if there is a missing value" in {
      val result = sortByKeyList(
        List(2, 1, 3),
        List((1, "123"), (3, "345"))
      )(_._1)
      result shouldEqual None
    }
  }
}
