package com.adamnfish.skull.integration

import com.adamnfish.skull.Skull.wake
import com.adamnfish.skull.models.Wake
import com.adamnfish.skull.{AttemptValues, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{OneInstancePerTest, OptionValues}


class WakeTest extends AnyFreeSpec with AttemptValues with OptionValues
  with SkullIntegration with OneInstancePerTest with TestHelpers {
  "for a valid request" - {
    val wakeRequest = Wake()

    "is successful" in {
      withTestContext { (context, _) =>
        val response = wake(
          wakeRequest,
          context("player-address".address)
        ).value().response.value
        response.message shouldEqual "ok"
      }
    }
  }
}
