package com.adamnfish.skull.validation

import com.adamnfish.skull.attempt.{Attempt, FailedAttempt, Failure}

object Validation {
  type Validator[A] = (A, String) => List[Failure]

  private[validation] def validate[A](a: A, context: String, validator: Validator[A]): Attempt[Unit] = {
    val failures = validator(a, context)
    if (failures.isEmpty) Attempt.unit
    else Attempt.Left(FailedAttempt(failures))
  }
}
