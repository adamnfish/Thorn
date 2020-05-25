package com.adamnfish.thorn.logic

import com.adamnfish.thorn.{AttemptValues, TestHelpers}
import com.adamnfish.thorn.logic.Play._
import com.adamnfish.thorn.models._
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class PlayTest extends AnyFreeSpec with Matchers with AttemptValues with OptionValues with TestHelpers{
  val creator = Players.newPlayer("creator", PlayerAddress("creator-address"))
  val player1 = Players.newPlayer("player1", PlayerAddress("player-1-address"))
  val player2 = Players.newPlayer("player1", PlayerAddress("player-1-address"))
  val player3 = Players.newPlayer("player3", PlayerAddress("player-3-address"))
  val game = Games.newGame("test game", creator)

  "advanceActivePlayer" - {
    val players = List(creator, player1, player2)
    "advances to the next player" in {
      advanceActivePlayer(players,
        creator.playerId
      ) shouldEqual player1.playerId
    }

    "advances from the middle of the pack" in {
      advanceActivePlayer(players,
        player1.playerId
      ) shouldEqual player2.playerId
    }

    "wraps to the first player if we're at the last player now" in {
      advanceActivePlayer(players,
        player2.playerId
      ) shouldEqual creator.playerId
    }

    "skips a player with no discs left" in {
      val players = List(
        creator,
        player1.copy(
          roseCount = 0,
          hasThorn = false,
        ),
        player2
      )
      advanceActivePlayer(players, creator.playerId) shouldEqual player2.playerId
    }

    "skips multiple players with no discs left" in {
      val players = List(
        creator,
        player1.copy(
          roseCount = 0,
          hasThorn = false,
        ),
        player2.copy(
          roseCount = 0,
          hasThorn = false,
        ),
        player3
      )
      advanceActivePlayer(players, creator.playerId) shouldEqual player3.playerId
    }
  }

  "placeDisc" - {
    "handles each possible round state" - {
      "fails if there is no round" in {
        placeDisc(Thorn, creator.playerId,
          game.copy(
            round = None
          )
        ).isFailedAttempt()
      }

      "for initial discs" - {
        "adds disc to the matching player's discs" in {
          val newGame = placeDisc(Thorn, creator.playerId,
            game.copy(
              players = List(
                creator, player1,
              ),
              round = Some(InitialDiscs(
                creator.playerId, Map.empty
              )),
            )
          ).value()
          newGame.round.value shouldBe a[InitialDiscs]
          val discs = newGame.round.value.asInstanceOf[InitialDiscs].initialDiscs.get(creator.playerId).value
          discs shouldEqual List(Thorn)
        }

        "fails to place a Rose that the player does not have" in {
          placeDisc(Rose, creator.playerId,
            game.copy(
              players = List(
                creator.copy(
                  roseCount = 0
                ),
                player1,
              ),
              round = Some(InitialDiscs(
                creator.playerId, Map.empty
              )),
            )
          ).isFailedAttempt()
        }

        "fails to place a Thorn that the player does not have" in {
          placeDisc(Thorn, creator.playerId,
            game.copy(
              players = List(
                creator.copy(
                  hasThorn = false
                ),
                player1,
              ),
              round = Some(InitialDiscs(
                creator.playerId, Map.empty
              )),
            )
          ).isFailedAttempt()
        }

        "does not change the first player" in {
          val newGame = placeDisc(Thorn, creator.playerId,
            game.copy(
              players = List(
                creator, player1,
              ),
              round = Some(InitialDiscs(
                creator.playerId, Map.empty
              )),
            )
          ).value()
          newGame.round.value shouldBe a[InitialDiscs]
          val firstPlayer = newGame.round.value.asInstanceOf[InitialDiscs].firstPlayer
          firstPlayer shouldEqual creator.playerId
        }

        "does not allow a second disc to be placed during the initial discs round" in {
          placeDisc(Thorn, creator.playerId,
            game.copy(
              round = Some(InitialDiscs(
                creator.playerId,
                Map(
                  creator.playerId -> List(Rose)
                ),
              ))
            )
          ).isFailedAttempt()
        }

        "if all players have placed their initial disc" - {
          "advances round to 'placing'" in {
            placeDisc(Thorn, creator.playerId,
              game.copy(
                round = Some(InitialDiscs(
                  creator.playerId,
                  Map(
                    creator.playerId -> Nil,
                    player1.playerId -> List(Rose),
                  ),
                ))
              )
            ).value().round.value shouldBe a[Placing]
          }

          "uses initial disc's player as active player" in {
            val activePlayer = creator.playerId
            val placingRound = placeDisc(Thorn, creator.playerId,
              game.copy(
                round = Some(InitialDiscs(
                  activePlayer,
                  Map(
                    creator.playerId -> Nil,
                    player1.playerId -> List(Rose),
                  ),
                ))
              )
            ).value().round.value.asInstanceOf[Placing]
            placingRound.activePlayer shouldEqual activePlayer
          }
        }
      }

      "for Placing" - {
        "adds disc to the matching player's discs" in {
          val newGame = placeDisc(Thorn, creator.playerId,
            game.copy(
              players = List(
                creator, player1,
              ),
              round = Some(Placing(
                creator.playerId, Map.empty
              )),
            )
          ).value()
          newGame.round.value shouldBe a[Placing]
          val discs = newGame.round.value.asInstanceOf[Placing].discs.get(creator.playerId).value
          discs shouldEqual List(Thorn)
        }

        "advances the active player in example 3-player game" - {
          "once" in {
            val newGame = placeDisc(Rose, creator.playerId,
              game.copy(
                players = List(
                  creator, player1, player2
                ),
                round = Some(Placing(
                  creator.playerId, Map.empty
                )),
              )
            ).value()
            newGame.round.value shouldBe a[Placing]
            val activePlayer = newGame.round.value.asInstanceOf[Placing].activePlayer
            activePlayer shouldEqual player1.playerId
          }

          "twice" in {
            val newGame1 = placeDisc(Rose, creator.playerId,
              game.copy(
                players = List(
                  creator, player1, player2
                ),
                round = Some(Placing(
                  creator.playerId, Map.empty
                )),
              )
            ).value()
            val newGame2 = placeDisc(Rose, player1.playerId, newGame1).value()
            newGame2.round.value shouldBe a[Placing]
            val activePlayer = newGame2.round.value.asInstanceOf[Placing].activePlayer
            activePlayer shouldEqual player2.playerId
          }

          "three times, back to the creator" in {
            val newGame1 = placeDisc(Rose, creator.playerId,
              game.copy(
                players = List(
                  creator, player1, player2
                ),
                round = Some(Placing(
                  creator.playerId, Map.empty
                )),
              )
            ).value()
            val newGame2 = placeDisc(Rose, player1.playerId, newGame1).value()
            val newGame3 = placeDisc(Rose, player2.playerId, newGame2).value()
            newGame3.round.value shouldBe a[Placing]
            val activePlayer = newGame3.round.value.asInstanceOf[Placing].activePlayer
            activePlayer shouldEqual creator.playerId
          }
        }

        "adds disc to the start of matching player's discs" in {
          val newGame = placeDisc(Thorn, creator.playerId,
            game.copy(
              round = Some(Placing(
                creator.playerId,
                Map(
                  creator.playerId -> List(Rose)
                ),
              ))
            )
          ).value()
          newGame.round.value shouldBe a[Placing]
          val discs = newGame.round.value.asInstanceOf[Placing].discs.get(creator.playerId).value
          discs shouldEqual List(Thorn, Rose)
        }

        "fails to place a Rose in excess of the player's remaining number" in {
          placeDisc(Rose, creator.playerId,
            game.copy(
              players = List(
                creator.copy(
                  roseCount = 3
                ),
                player1,
              ),
              round = Some(Placing(
                creator.playerId,
                Map(creator.playerId -> List(Rose, Rose, Rose)),
              )),
            )
          ).isFailedAttempt()
        }

        "fails to place a Thorn that the player does not have" in {
          placeDisc(Thorn, creator.playerId,
            game.copy(
              players = List(
                creator.copy(
                  hasThorn = false
                ),
                player1,
              ),
              round = Some(Placing(
                creator.playerId, Map.empty
              )),
            )
          ).isFailedAttempt()
        }

        "fails to place a second Thorn, even if the player has one" in {
          placeDisc(Thorn, creator.playerId,
            game.copy(
              players = List(
                creator.copy(
                  hasThorn = true
                ),
                player1,
              ),
              round = Some(Placing(
                creator.playerId,
                Map(
                  creator.playerId -> List(Thorn)
                ),
              )),
            )
          ).isFailedAttempt()
        }

        "fails if this is not the active player" in {
          placeDisc(Thorn, player1.playerId,
            game.copy(
              players = List(
                creator, player1,
              ),
              round = Some(Placing(
                creator.playerId,
                Map.empty,
              ))
            )
          ).isFailedAttempt()
        }
      }

      "fails if the round is bidding" in {
        placeDisc(Thorn, creator.playerId,
          game.copy(
            round = Some(Bidding(creator.playerId, Map.empty, Map.empty, Nil))
          )
        ).isFailedAttempt()
      }

      "fails if the round is flipping" in {
        placeDisc(Thorn, creator.playerId,
          game.copy(
            round = Some(Flipping(creator.playerId, 3, Map.empty, Map.empty, Map.empty))
          )
        ).isFailedAttempt()
      }

      "fails if the round is finished" in {
        placeDisc(Thorn, creator.playerId,
          game.copy(
            round = Some(Finished(creator.playerId, Map.empty, Map.empty, false))
          )
        ).isFailedAttempt()
      }
    }
  }

  "allPlayersPlaced" - {
    "returns true of all players have placed discs" in {
      allPlayersPlaced(
        Map(
          creator.playerId -> List(Rose),
          player1.playerId -> List(Rose),
          player2.playerId -> List(Rose),
        ),
        List(creator, player1, player2)
      ) shouldEqual true
    }

    "returns false if players have not placed discs" ignore {
      allPlayersPlaced(
        Map(
          creator.playerId -> List(Rose),
          player1.playerId -> List(Rose),
          player2.playerId -> Nil,
        ),
        List(creator, player1, player2)
      ) shouldEqual false
    }

    "returns true if only dead players are yet to place" ignore {
      allPlayersPlaced(
        Map(
          creator.playerId -> List(Rose),
          player1.playerId -> List(Rose),
          player2.playerId -> Nil,
        ),
        List(creator, player1,
          player2.copy(
            roseCount = 0,
            hasThorn = false,
          )
        )
      ) shouldEqual false
    }
  }

  "bidOnRound" - {
    "handles each round" - {
      "fails for no round" in {
        bidOnRound(1, creator.playerId,
          game.copy(
            round = None
          )
        ).isFailedAttempt()
      }

      "fails for initial discs" in {
        bidOnRound(1, creator.playerId,
          game.copy(
            round = Some(InitialDiscs(
              creator.playerId, Map.empty
            ))
          )
        ).isFailedAttempt()
      }

      "for placing" - {
        "fails if it is not the player's turn" in {
          bidOnRound(3, creator.playerId,
            game.copy(
              players = List(
                creator, player1,
              ),
              round = Some(Placing(
                player1.playerId, Map.empty
              ))
            )
          ).isFailedAttempt()
        }

        "if it is the player's turn" - {
          val testGame = game.copy(
            players = List(creator, player1, player2),
            round = Some(Placing(
              creator.playerId,
              Map(
                creator.playerId -> List(Rose, Rose),
                player1.playerId -> List(Rose, Rose),
                player2.playerId -> List(Thorn, Rose),
              ),
            ))
          )
          "advances the round to 'bidding'" in {
            val result = bidOnRound(3, creator.playerId, testGame).value()
            result.round.value shouldBe a[Bidding]
          }

          "updates player bid to the specified amount" in {
            val result = bidOnRound(3, creator.playerId, testGame).value()
            val bid = result.round.value.asInstanceOf[Bidding].bids.get(creator.playerId).value
            bid shouldEqual 3
          }

          "advances the active player" in {
            val result = bidOnRound(3, creator.playerId, testGame).value()
            result.round.value shouldBe a[Bidding]
            val activePlayer = result.round.value.asInstanceOf[Bidding].activePlayer
            activePlayer shouldEqual player1.playerId
          }

          "fails if the bid exceeds the number of discs" in {
            bidOnRound(8, creator.playerId, testGame).isFailedAttempt()
          }
        }
      }

      "for bidding" - {
        val testGame = game.copy(
          players = List(creator, player1),
          round = Some(Bidding(
            creator.playerId, Map(
              creator.playerId -> List(Rose, Rose),
              player1.playerId -> List(Thorn, Rose),
            ), Map.empty, Nil
          ))
        )

        "sets player bid to the specified amount if they had no previous bid" in {
          val result = bidOnRound(1, creator.playerId, testGame).value()
          val bid = result.round.value.asInstanceOf[Bidding].bids.get(creator.playerId).value
          bid shouldEqual 1
        }

        "if the bid is equal to the number of discs" - {
          "advances to flipping" in {
            val result = bidOnRound(4, creator.playerId, testGame).value()
            result.round.value.asInstanceOf[Flipping]
          }

          "sets up new round correctly" in {
            val result = bidOnRound(4, creator.playerId, testGame).value()
            val flipping = result.round.value.asInstanceOf[Flipping]
            flipping should have(
              "activePlayer" as creator.playerId.pid,
              "target" as 4,
              "discs" as Map(
                creator.playerId -> List(Rose, Rose),
                player1.playerId -> List(Thorn, Rose),
              )
            )
          }

          "includes player bid on the new round" in {
            val result = bidOnRound(4, creator.playerId, testGame).value()
            val flipping = result.round.value.asInstanceOf[Flipping]
            flipping.bids.get(creator.playerId).value shouldEqual 4
          }
        }

        "updates player bid to the specified amount" in {
          val result = bidOnRound(3, creator.playerId,
            game.copy(
              round = Some(Bidding(
                creator.playerId,
                Map(
                  creator.playerId -> List(Rose, Rose),
                  player1.playerId -> List(Thorn, Rose),
                ),
                Map(
                  creator.playerId -> 1
                ),
                Nil
              ))
            )
          ).value()
          val bid = result.round.value.asInstanceOf[Bidding].bids.get(creator.playerId).value
          bid shouldEqual 3
        }

        "advances the active player" in {
          val result = bidOnRound(3, creator.playerId, testGame).value()
          val activePlayer = result.round.value.asInstanceOf[Bidding].activePlayer
          activePlayer shouldEqual player1.playerId
        }

        "fails if the bid is lower than the player's previous bid" in {
          bidOnRound(3, creator.playerId,
            game.copy(
              round = Some(Bidding(
                creator.playerId,
                Map(
                  creator.playerId -> List(Rose, Rose),
                  player1.playerId -> List(Thorn, Rose),
                ),
                Map(
                  creator.playerId -> 5
                ),
                Nil
              ))
            )
          ).isFailedAttempt()
        }

        "fails if the bid is lower than another player's previous bid" in {
          bidOnRound(3, creator.playerId,
            game.copy(
              players = List(
                creator, player1,
              ),
              round = Some(Bidding(
                creator.playerId,
                Map(
                  creator.playerId -> List(Rose, Rose),
                  player1.playerId -> List(Thorn, Rose),
                ),
                Map(
                  creator.playerId -> 1,
                  player1.playerId -> 5,
                ),
                Nil
              ))
            )
          ).isFailedAttempt()
        }

        "fails if the bid exceeds the number of discs" in {
          bidOnRound(5, creator.playerId, testGame).isFailedAttempt()
        }

        "fails if it is not the player's turn" in {
          bidOnRound(3, creator.playerId,
            game.copy(
              players = List(
                creator, player1,
              ),
              round = Some(Bidding(
                player1.playerId,
                Map(
                  creator.playerId -> List(Rose, Rose),
                  player1.playerId -> List(Thorn, Rose),
                ),
                Map.empty, Nil
              ))
            )
          ).isFailedAttempt()
        }

        "fails if the player has passed" in {
          bidOnRound(3, creator.playerId,
            game.copy(
              round = Some(Bidding(
                creator.playerId,
                Map(
                  creator.playerId -> List(Rose, Rose),
                  player1.playerId -> List(Thorn, Rose),
                ),
                Map.empty,
                List(creator.playerId)
              ))
            )
          ).isFailedAttempt()
        }
      }

      "fails for flipping" in {
        bidOnRound(1, creator.playerId,
          game.copy(
            round = Some(Flipping(
              creator.playerId, 3, Map.empty, Map.empty, Map.empty
            ))
          )
        ).isFailedAttempt()
      }

      "fails for finished" in {
        bidOnRound(1, creator.playerId,
          game.copy(
            round = Some(Finished(
              creator.playerId, Map.empty, Map.empty, false
            ))
          )
        ).isFailedAttempt()
      }
    }
  }

  "passRound" - {
    "handles each round" - {
      "fails for no round" in {
        passRound(creator.playerId,
          game.copy(
            round = None
          )
        ).isFailedAttempt()
      }

      "fails for initial discs" in {
        passRound(creator.playerId,
          game.copy(
            round = Some(InitialDiscs(
              creator.playerId, Map.empty
            ))
          )
        ).isFailedAttempt()
      }

      "fails for placing" in {
        passRound(creator.playerId,
          game.copy(
            round = Some(Placing(
              creator.playerId, Map.empty
            ))
          )
        ).isFailedAttempt()
      }

      "for bidding" - {
        "adds this player to an empty list of passed players" in {
          val result = passRound(
            creator.playerId,
            game.copy(
              players = List(creator, player1, player2),
              round = Some(Bidding(
                activePlayer = creator.playerId,
                discs = Map(
                  creator.playerId -> List(Rose, Rose),
                  player1.playerId -> List(Rose, Thorn),
                  player2.playerId -> List(Thorn, Rose),
                ),
                bids = Map(
                  player2.playerId -> 2,
                ),
                passed = Nil,
              ))
            )
          ).value()
          val round = result.round.value
          round shouldBe a[Bidding]
          round.asInstanceOf[Bidding].passed should contain(creator.playerId)
        }

        "advances the active player in example three player game" in {
          val result = passRound(
            creator.playerId,
            game.copy(
              players = List(creator, player1, player2),
              round = Some(Bidding(
                activePlayer = creator.playerId,
                discs = Map(
                  creator.playerId -> List(Rose, Rose),
                  player1.playerId -> List(Rose, Thorn),
                  player2.playerId -> List(Thorn, Rose),
                ),
                bids = Map(
                  player2.playerId -> 2,
                ),
                passed = Nil,
              ))
            )
          ).value()
          val round = result.round.value
          round shouldBe a[Bidding]
          round.asInstanceOf[Bidding].activePlayer shouldEqual player1.playerId
        }

        "adds this player to an existing list of passed players" in {
          val result = passRound(
            creator.playerId,
            game.copy(
              players = List(creator, player1, player2, player3),
              round = Some(Bidding(
                activePlayer = creator.playerId,
                discs = Map(
                  creator.playerId -> List(Rose, Rose),
                  player1.playerId -> List(Rose, Thorn),
                  player2.playerId -> List(Thorn, Rose),
                  player3.playerId -> List(Thorn, Rose),
                ),
                bids = Map(
                  player2.playerId -> 2,
                ),
                passed = List(player3.playerId),
              ))
            )
          ).value()
          val round = result.round.value
          result.round.value shouldBe a[Bidding]
          round.asInstanceOf[Bidding].passed should contain allOf(creator.playerId, player3.playerId)
        }

        "advances the round to 'flipping' if this is the last player to pass" in {
          val discs = Map(
            creator.playerId -> List(Rose, Rose),
            player1.playerId -> List(Rose, Thorn),
            player2.playerId -> List(Thorn, Rose),
          )
          val result = passRound(
            creator.playerId,
            game.copy(
              players = List(creator, player1, player2),
              round = Some(Bidding(
                activePlayer = creator.playerId,
                discs = discs,
                bids = Map(
                  player2.playerId -> 2,
                ),
                passed = List(
                  player1.playerId,
                ),
              ))
            )
          ).value()
          val round = result.round.value
          round shouldBe a[Flipping]
          round.asInstanceOf[Flipping] should have(
            "activePlayer" as player2.playerId.pid,
            "discs" as discs,
            "target" as 2,
            "bids" as Map(
              player2.playerId -> 2,
            ),
            "revealed" as Map.empty,
          )
        }

        "fails if it is not the player's turn" in {
          passRound(
            creator.playerId,
            game.copy(
              players = List(creator, player1, player2),
              round = Some(Bidding(
                activePlayer = player1.playerId,
                discs = Map(
                  creator.playerId -> List(Rose, Rose),
                  player1.playerId -> List(Rose, Thorn),
                  player2.playerId -> List(Thorn, Rose),
                ),
                bids = Map(
                  creator.playerId -> 2,
                ),
                passed = Nil,
              ))
            )
          ).isFailedAttempt()
        }

        "fails if the player has already passed" in {
          passRound(
            creator.playerId,
            game.copy(
              players = List(creator, player1, player2),
              round = Some(Bidding(
                activePlayer = creator.playerId,
                discs = Map(
                  creator.playerId -> List(Rose, Rose),
                  player1.playerId -> List(Rose, Thorn),
                  player2.playerId -> List(Thorn, Rose),
                ),
                bids = Map(
                  player2.playerId -> 2,
                ),
                passed = List(creator.playerId),
              ))
            )
          ).isFailedAttempt()
        }
      }

      "fails for flipping" in {
        passRound(creator.playerId,
          game.copy(
            round = Some(Flipping(
              creator.playerId, 3, Map.empty, Map.empty, Map.empty
            ))
          )
        ).isFailedAttempt()
      }

      "fails for finished" in {
        passRound(creator.playerId,
          game.copy(
            round = Some(Finished(
              creator.playerId, Map.empty, Map.empty, false
            ))
          )
        ).isFailedAttempt()
      }
    }
  }

  "biddingRoundWinner" - {
    "returns the hotseat player ID" - {
      "for a 2P game where the other player has passed" in {
        val hotseatPlayerId = biddingRoundWinner(
          passed = List(player1.playerId),
          bids = Map(
            player2.playerId -> 2,
          ),
          players = List(player1, player2),
        ).value
        hotseatPlayerId shouldEqual (player2.playerId, 2)
      }

      "for a 3P game where both the other players have passed" in {
        val hotseatPlayerId = biddingRoundWinner(
          passed = List(player1.playerId, player2.playerId),
          bids = Map(
            creator.playerId -> 3,
            player1.playerId -> 2,
          ),
          players = List(creator, player1, player2),
        ).value
        hotseatPlayerId shouldEqual (creator.playerId, 3)
      }

      "for a 4P game where everyone else has passed" in {
        val hotseatPlayerId = biddingRoundWinner(
          passed = List(player1.playerId, player2.playerId, player3.playerId),
          bids = Map(
            creator.playerId -> 3,
          ),
          players = List(creator, player1, player2, player3),
        ).value
        hotseatPlayerId shouldEqual (creator.playerId, 3)
      }

      "for a 4P game where only dead players remain" in {
        val hotseatPlayerId = biddingRoundWinner(
          passed = List(player1.playerId, player2.playerId),
          bids = Map(
            creator.playerId -> 3,
          ),
          players = List(creator, player1, player2,
            player3.copy(
              roseCount = 0,
              hasThorn = false,
            )
          ),
        ).value
        hotseatPlayerId shouldEqual (creator.playerId, 3)
      }
    }

    "returns None" - {
      "when another player is yet to act" in {
        val result = biddingRoundWinner(
          passed = List(player1.playerId),
          bids = Map(
            creator.playerId -> 3,
          ),
          players = List(creator, player1, player2),
        )
        result shouldEqual None
      }

      "when all players are still bidding" in {
        val result = biddingRoundWinner(
          passed = Nil,
          bids = Map(
            creator.playerId -> 3,
            player1.playerId -> 4,
            player2.playerId -> 5,
            player3.playerId -> 6,
          ),
          players = List(creator, player1, player2, player3),
        )
        result shouldEqual None
      }

      "when two players are still bidding" in {
        val result = biddingRoundWinner(
          passed = List(creator.playerId, player2.playerId),
          bids = Map(
            creator.playerId -> 3,
            player1.playerId -> 4,
            player2.playerId -> 5,
            player3.playerId -> 6,
          ),
          players = List(creator, player1, player2, player3),
        )
        result shouldEqual None
      }
    }
  }

  "flipDisc" - {
    "handles each round" - {
      "fails for no round" in {
        flipDisc(creator.playerId, stack = player1.playerId,
          game.copy(
            players = List(creator, player1, player2),
            round = None,
          )
        ).isFailedAttempt()
      }

      "fails for initial discs" in {
        flipDisc(creator.playerId, stack = player1.playerId,
          game.copy(
            players = List(creator, player1, player2),
            round = Some(InitialDiscs(
              firstPlayer = creator.playerId,
              initialDiscs = Map(
                creator.playerId -> List(Rose),
                player1.playerId -> List(Rose),
                player2.playerId -> List(Rose),
              ),
            )),
          )
        ).isFailedAttempt()
      }

      "fails for placing" in {
        flipDisc(creator.playerId, stack = player1.playerId,
          game.copy(
            players = List(creator, player1, player2),
            round = Some(Placing(
              activePlayer = creator.playerId,
              discs = Map(
                creator.playerId -> List(Rose),
                player1.playerId -> List(Rose),
                player2.playerId -> List(Rose),
              ),
            )),
          )
        ).isFailedAttempt()
      }

      "fails for bidding" in {
        flipDisc(creator.playerId, stack = player1.playerId,
          game.copy(
            players = List(creator, player1, player2),
            round = Some(Bidding(
              activePlayer = creator.playerId,
              discs = Map(
                creator.playerId -> List(Rose),
                player1.playerId -> List(Rose),
                player2.playerId -> List(Rose),
              ),
              bids = Map.empty,
              passed = Nil
            )),
          )
        ).isFailedAttempt()
      }

      "for flipping" - {
        "if the revealed disc is a Rose" - {
          "if this meets the player's bid amount" - {
            val testGame = game.copy(
              players = List(creator, player1, player2),
              round = Some(Flipping(
                activePlayer = creator.playerId,
                discs = Map(
                  creator.playerId -> List(Rose),
                  player1.playerId -> List(Rose),
                  player2.playerId -> List(Rose),
                ),
                target = 2,
                bids = Map(
                  creator.playerId -> 2,
                ),
                revealed = Map(
                  creator.playerId -> List(Rose),
                ),
              ))
            )

            "advances the round to finished" in {
              val result = flipDisc(creator.playerId, player1.playerId, testGame).value()
              result.round.value shouldBe a[Finished]
            }

            "correctly populates the new round" in {
              val result = flipDisc(creator.playerId, player1.playerId, testGame).value()
              result.round.value shouldBe a[Finished]
              val finishedRound = result.round.value.asInstanceOf[Finished]
              finishedRound should have(
                "activePlayer" as creator.playerId.pid,
                "discs" as Map(
                  creator.playerId -> List(Rose),
                  player1.playerId -> List(Rose),
                  player2.playerId -> List(Rose),
                ),
                "revealed" as Map(
                  creator.playerId -> List(Rose),
                  player1.playerId -> List(Rose),
                ),
                "successful" as true,
              )
            }
          }

          "if this does not meet the player's bid amount" - {
            val testGame = game.copy(
              players = List(creator, player1, player2),
              round = Some(Flipping(
                activePlayer = creator.playerId,
                discs = Map(
                  creator.playerId -> List(Rose),
                  player1.playerId -> List(Rose, Rose),
                  player2.playerId -> List(Rose),
                ),
                bids = Map(
                  creator.playerId -> 4,
                ),
                target = 4,
                revealed = Map(
                  creator.playerId -> List(Rose),
                ),
              ))
            )

            "reveals the flipped Rose" in {
              val result = flipDisc(creator.playerId, player1.playerId, testGame).value()
              result.round.value shouldBe a[Flipping]
              val flippingRound = result.round.value.asInstanceOf[Flipping]
              flippingRound.revealed.get(player1.playerId).value shouldEqual List(Rose)
            }

            "reveals multiple flipped discs" in {
              val game2 = flipDisc(creator.playerId, player1.playerId, testGame).value()
              val result = flipDisc(creator.playerId, player2.playerId, game2).value()
              result.round.value shouldBe a[Flipping]
              val flippingRound = result.round.value.asInstanceOf[Flipping]
              flippingRound.revealed shouldEqual Map(
                creator.playerId -> List(Rose),
                player1.playerId -> List(Rose),
                player2.playerId -> List(Rose),
              )
            }
          }
        }

        "if the revealed disc is a Thorn" - {
          val testRound = Flipping(
            activePlayer = creator.playerId,
            discs = Map(
              creator.playerId -> List(Rose),
              player1.playerId -> List(Rose),
              player2.playerId -> List(Rose, Thorn),
            ),
            bids = Map(
              creator.playerId -> 3,
            ),
            target = 3,
            revealed = Map(
              creator.playerId -> List(Rose),
              player2.playerId -> List(Rose),
            ),
          )
          val testGame = game.copy(
            players = List(creator, player1, player2),
            round = Some(testRound)
          )

          "advances the round to finished" in {
            val result = flipDisc(creator.playerId, player2.playerId, testGame).value()
            result.round.value shouldBe a[Finished]
          }

          "correctly populates the finished round" in {
            val result = flipDisc(creator.playerId, player2.playerId, testGame).value()
            result.round.value shouldBe a[Finished]
            val finishedRound = result.round.value.asInstanceOf[Finished]
            finishedRound should have(
              "activePlayer" as creator.playerId.pid,
              "discs" as Map(
                creator.playerId -> List(Rose),
                player1.playerId -> List(Rose),
                player2.playerId -> List(Rose, Thorn),
              ),
              "revealed" as Map(
                creator.playerId -> List(Rose),
                player2.playerId -> List(Rose, Thorn),
              ),
              "successful" as false,
            )
          }

          "removes a disc from the player's pool" in {
            sealed trait RemovedDisc
            object RoseRemoved extends RemovedDisc
            object ThornRemoved extends RemovedDisc

            // repeat to allow rng to remove Thorns and Roses
            val results = runMultiple[RemovedDisc](50) { _ =>
              val result = flipDisc(creator.playerId, player2.playerId, testGame).value()
              val updatedPlayer = result.players.find(_.playerId == creator.playerId).value
              (updatedPlayer.roseCount, updatedPlayer.hasThorn) match {
                case (3, true) =>
                  Left("neither rose nor skull removed from players pool")
                case (3, false) =>
                  Right(ThornRemoved)
                case (2, true) =>
                  Right(RoseRemoved)
                case (roseCount, hasThorn) =>
                  Left(s"Too many discs removed - roses: $roseCount, thorn: $hasThorn")
              }
            }

            results.fold(
              { errs =>
                fail(errs.distinct.mkString(", "))
              },
              { values =>
                values should (contain(RoseRemoved) and contain(ThornRemoved))
              }
            )
          }
        }

        "players must flip their own discs first" - {
          val testRound = Flipping(
            activePlayer = creator.playerId,
            discs = Map(
              creator.playerId -> List(Rose, Rose),
              player1.playerId -> List(Rose, Rose),
              player2.playerId -> List(Rose),
            ),
            bids = Map(
              creator.playerId -> 4,
            ),
            target = 4,
            revealed = Map.empty,
          )
          "fails if they haven't flipped any of their own discs" in {
            flipDisc(creator.playerId, stack = player2.playerId,
              game.copy(
                players = List(creator, player1, player2),
                round = Some(testRound),
              )
            ).isFailedAttempt()
          }

          "fails if they have not flipped all their discs" in {
            flipDisc(creator.playerId, stack = player2.playerId,
              game.copy(
                players = List(creator, player1, player2),
                round = Some(testRound.copy(
                  revealed = Map(
                    creator.playerId -> List(Rose)
                  )
                )),
              )
            ).isFailedAttempt()
          }
        }

        "cannot flip discs beyond what has been placed" - {
          "for themselves" in {
            flipDisc(creator.playerId, stack = creator.playerId,
              game.copy(
                players = List(creator, player1, player2),
                round = Some(Flipping(
                  activePlayer = creator.playerId,
                  discs = Map(
                    creator.playerId -> List(Rose),
                    player1.playerId -> List(Rose),
                    player2.playerId -> List(Rose),
                  ),
                  target = 3,
                  bids = Map(
                    creator.playerId -> 3,
                  ),
                  revealed = Map(
                    creator.playerId -> List(Rose),
                  ),
                )),
              )
            ).isFailedAttempt()
          }

          "and for other players" in {
            flipDisc(creator.playerId, stack = player1.playerId,
              game.copy(
                players = List(creator, player1, player2),
                round = Some(Flipping(
                  activePlayer = creator.playerId,
                  discs = Map(
                    creator.playerId -> List(Rose),
                    player1.playerId -> List(Rose),
                    player2.playerId -> List(Rose),
                  ),
                  target = 3,
                  bids = Map(
                    creator.playerId -> 3,
                  ),
                  revealed = Map(
                    creator.playerId -> List(Rose),
                    player1.playerId -> List(Rose),
                  ),
                )),
              )
            ).isFailedAttempt()
          }
        }

        "fails if this is not the active player" in {
          flipDisc(creator.playerId, stack = creator.playerId,
            game.copy(
              players = List(creator, player1, player2),
              round = Some(Flipping(
                activePlayer = player2.playerId,
                discs = Map(
                  creator.playerId -> List(Rose),
                  player1.playerId -> List(Rose),
                  player2.playerId -> List(Rose),
                ),
                target = 2,
                bids = Map(
                  creator.playerId -> 2,
                ),
                revealed = Map.empty,
              ))
            )
          ).isFailedAttempt()
        }
      }

      "fails for finished" in {
        flipDisc(creator.playerId, stack = player1.playerId,
          game.copy(
            players = List(creator, player1, player2),
            round = Some(Finished(
              activePlayer = creator.playerId,
              discs = Map(
                creator.playerId -> List(Rose),
                player1.playerId -> List(Rose),
                player2.playerId -> List(Rose),
              ),
              revealed = Map(
                creator.playerId -> List(Rose),
                player1.playerId -> List(Rose),
                player2.playerId -> List(Rose),
              ),
              successful = true,
            )),
          )
        ).isFailedAttempt()
      }
    }
  }
}
