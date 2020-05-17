package com.adamnfish.thorn.logic

import com.adamnfish.thorn.AttemptValues
import com.adamnfish.thorn.logic.Play._
import com.adamnfish.thorn.models._
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class PlayTest extends AnyFreeSpec with Matchers with AttemptValues with OptionValues {
  val creator = Players.newPlayer("creator", PlayerAddress("creator-address"))
  val player1 = Players.newPlayer("player1", PlayerAddress("player-1-address"))
  val player2 = Players.newPlayer("player1", PlayerAddress("player-1-address"))
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

          "uses initial disc's player as" in {
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
            round = Some(Flipping(creator.playerId, Map.empty, Map.empty))
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
          "advances the round to 'bidding'" in {
            val result = bidOnRound(3, creator.playerId,
              game.copy(
                round = Some(Placing(
                  creator.playerId, Map.empty
                ))
              )
            ).value()
            result.round.value shouldBe a[Bidding]
          }

          "updates player bid to the specified amount" in {
            val result = bidOnRound(3, creator.playerId,
              game.copy(
                round = Some(Placing(
                  creator.playerId, Map.empty
                ))
              )
            ).value()
            val bid = result.round.value.asInstanceOf[Bidding].bids.get(creator.playerId).value
            bid shouldEqual 3
          }

          "advances the active player" in {
            val result = bidOnRound(3, creator.playerId,
              game.copy(
                players = List(creator, player1),
                round = Some(Placing(
                  creator.playerId, Map.empty
                ))
              )
            ).value()
            result.round.value shouldBe a[Bidding]
            val activePlayer = result.round.value.asInstanceOf[Bidding].activePlayer
            activePlayer shouldEqual player1.playerId
          }
        }
      }

      "for bidding" - {
        "sets player bid to the specified amount if they had no previous bid" in {
          val result = bidOnRound(1, creator.playerId,
            game.copy(
              round = Some(Bidding(
                creator.playerId, Map.empty, Map.empty, Nil
              ))
            )
          ).value()
          val bid = result.round.value.asInstanceOf[Bidding].bids.get(creator.playerId).value
          bid shouldEqual 1
        }

        "updates player bid to the specified amount" in {
          val result = bidOnRound(3, creator.playerId,
            game.copy(
              round = Some(Bidding(
                creator.playerId, Map.empty,
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
          val result = bidOnRound(3, creator.playerId,
            game.copy(
              players = List(
                creator, player1
              ),
              round = Some(Bidding(
                creator.playerId, Map.empty, Map.empty, Nil
              ))
            )
          ).value()
          val activePlayer = result.round.value.asInstanceOf[Bidding].activePlayer
          activePlayer shouldEqual player1.playerId
        }

        "fails if the bid is lower than the player's previous bid" in {
          bidOnRound(3, creator.playerId,
            game.copy(
              round = Some(Bidding(
                creator.playerId, Map.empty,
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
                creator.playerId, Map.empty,
                Map(
                  creator.playerId -> 1,
                  player1.playerId -> 5,
                ),
                Nil
              ))
            )
          ).isFailedAttempt()
        }

        "fails if it is not the player's turn" in {
          bidOnRound(3, creator.playerId,
            game.copy(
              players = List(
                creator, player1,
              ),
              round = Some(Bidding(
                player1.playerId, Map.empty, Map.empty, Nil
              ))
            )
          ).isFailedAttempt()
        }

        "fails if the player has passed" in {
          bidOnRound(3, creator.playerId,
            game.copy(
              round = Some(Bidding(
                creator.playerId, Map.empty, Map.empty,
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
              creator.playerId, Map.empty, Map.empty
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
}
