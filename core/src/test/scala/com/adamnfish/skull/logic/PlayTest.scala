package com.adamnfish.skull.logic

import com.adamnfish.skull.AttemptValues
import com.adamnfish.skull.logic.Play._
import com.adamnfish.skull.models._
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class PlayTest extends AnyFreeSpec with Matchers with AttemptValues with OptionValues {
  val creator = Players.newPlayer("creator", PlayerAddress("creator-address"))
  val player1 = Players.newPlayer("player1", PlayerAddress("player-1-address"))
  val game = Games.newGame("test game", creator)

  "placeDisc" - {
    "handles each possible round state" - {
      "fails if there is no round" in {
        placeDisc(Skull, creator.playerId,
          game.copy(
            round = None
          )
        ).isFailedAttempt()
      }

      "for initial discs" - {
        "adds disc to the matching player's discs" in {
          val newGame = placeDisc(Skull, creator.playerId,
            game.copy(
              players = Map(
                creator.playerId -> creator,
                player1.playerId -> player1,
              ),
              round = Some(InitialDiscs(
                creator.playerId, Map.empty
              )),
            )
          ).value()
          newGame.round.value shouldBe a[InitialDiscs]
          val discs = newGame.round.value.asInstanceOf[InitialDiscs].initialDiscs.get(creator.playerId).value
          discs shouldEqual List(Skull)
        }

        "does not allow a second disc to be placed during the initial discs round" in {
          placeDisc(Skull, creator.playerId,
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

        "if all players haveplaced their initial disc" - {
          "advances round to 'placing'" in {
            placeDisc(Skull, creator.playerId,
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
            val placingRound = placeDisc(Skull, creator.playerId,
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
          val newGame = placeDisc(Skull, creator.playerId,
            game.copy(
              players = Map(
                creator.playerId -> creator,
                player1.playerId -> player1,
              ),
              round = Some(Placing(
                creator.playerId, Map.empty
              )),
            )
          ).value()
          newGame.round.value shouldBe a[Placing]
          val discs = newGame.round.value.asInstanceOf[Placing].discs.get(creator.playerId).value
          discs shouldEqual List(Skull)
        }

        "adds disc to the start of matching player's discs" in {
          val newGame = placeDisc(Skull, creator.playerId,
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
          discs shouldEqual List(Skull, Rose)
        }

        "fails if this is not the active player" in {
          placeDisc(Skull, player1.playerId,
            game.copy(
              players = Map(
                creator.playerId -> creator,
                player1.playerId -> player1,
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
        placeDisc(Skull, creator.playerId,
          game.copy(
            round = Some(Bidding(creator.playerId, Map.empty, Map.empty, Nil))
          )
        ).isFailedAttempt()
      }

      "fails if the round is flipping" in {
        placeDisc(Skull, creator.playerId,
          game.copy(
            round = Some(Flipping(creator.playerId, Map.empty, Map.empty))
          )
        ).isFailedAttempt()
      }

      "fails if the round is finished" in {
        placeDisc(Skull, creator.playerId,
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

      "fails for placing" in {
        bidOnRound(1, creator.playerId,
          game.copy(
            round = Some(Placing(
              creator.playerId, Map.empty
            ))
          )
        ).isFailedAttempt()
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

        "fails if the bid is lower than the player's previous bid" in {
          val result = bidOnRound(3, creator.playerId,
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
          val result = bidOnRound(3, creator.playerId,
            game.copy(
              players = Map(
                creator.playerId -> creator,
                player1.playerId -> player1,
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
          val result = bidOnRound(3, creator.playerId,
            game.copy(
              players = Map(
                creator.playerId -> creator,
                player1.playerId -> player1,
              ),
              round = Some(Bidding(
                player1.playerId, Map.empty, Map.empty, Nil
              ))
            )
          ).isFailedAttempt()
        }

        "fails if the player has passed" in {
          val result = bidOnRound(3, creator.playerId,
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
