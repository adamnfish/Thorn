package com.adamnfish.skull.logic

import org.scalatest.freespec.AnyFreeSpec
import Players._
import com.adamnfish.skull.models.PlayerAddress
import org.scalatest.matchers.should.Matchers


class PlayersTest extends AnyFreeSpec with Matchers {
  "newPlayer" - {
    val screenName = "screen-name"
    val address = PlayerAddress("address")

    "uses provided screen name" in {
      newPlayer(screenName, address).screenName shouldEqual screenName
    }

    "uses provided address" in {
      newPlayer(screenName, address).playerAddress shouldEqual address
    }

    "sets score to 0" in {
      newPlayer(screenName, address).score shouldEqual 0
    }

    "sets a random ID" in {
      val id1 = newPlayer(screenName, address).playerId
      val id2 = newPlayer(screenName, address).playerId
      id1 should not equal id2
    }

    "sets a random key" in {
      val key1 = newPlayer(screenName, address).playerKey
      val key2 = newPlayer(screenName, address).playerKey
      key1 should not equal key2
    }
  }
}
