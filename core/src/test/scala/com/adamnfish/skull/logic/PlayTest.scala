package com.adamnfish.skull.logic

import com.adamnfish.skull.AttemptValues
import com.adamnfish.skull.logic.Play._
import com.adamnfish.skull.models._
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class PlayTest extends AnyFreeSpec with Matchers with AttemptValues with OptionValues {
  val creator = Players.newPlayer("creator", PlayerAddress("creator-address"))
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
              round = Some(InitialDiscs(
                creator.playerId, Map.empty
              ))
            )
          ).value()
          newGame.round.value shouldBe a[InitialDiscs]
          val discs = newGame.round.value.asInstanceOf[InitialDiscs].initialDiscs.get(creator.playerId).value
          discs shouldEqual List(Skull)
        }

        "adds disc to the start of matching player's discs" in {
          val newGame = placeDisc(Skull, creator.playerId,
            game.copy(
              round = Some(InitialDiscs(
                creator.playerId,
                Map(
                  creator.playerId -> List(Rose)
                ),
              ))
            )
          ).value()
          newGame.round.value shouldBe a[InitialDiscs]
          val discs = newGame.round.value.asInstanceOf[InitialDiscs].initialDiscs.get(creator.playerId).value
          discs shouldEqual List(Skull, Rose)
        }
      }

      "for Placing" - {
        "adds disc to the matching player's discs" in {
          val newGame = placeDisc(Skull, creator.playerId,
            game.copy(
              round = Some(Placing(
                creator.playerId, Map.empty
              ))
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
}
