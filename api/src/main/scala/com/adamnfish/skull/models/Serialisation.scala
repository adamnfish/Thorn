package com.adamnfish.skull.models

import com.adamnfish.skull.attempt.FailedAttempt


object Serialisation {
  object DB {

  }

  object Application {

  }

  object Transport {
    def encodeMessage(message: Message): String = ???
    def encodeFailure(failures: FailedAttempt): String = ???
  }
}
