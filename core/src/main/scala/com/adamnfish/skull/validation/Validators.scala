package com.adamnfish.skull.validation

import com.adamnfish.skull.attempt.{Attempt, FailedAttempt, Failure}
import com.adamnfish.skull.models.CreateGame
import com.adamnfish.skull.validation.Validation.Validator

import scala.concurrent.ExecutionContext


object Validators {
  val nonEmpty: Validator[String] = { (iter, context) =>
    if (iter.isEmpty) {
      List(
        Failure("Validation failure: empty", s"$context is required", 400, Some(context))
      )
    } else Nil
  }

  private val uuidPattern = "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}".r
  val isUUID: Validator[String] = { (str, context) =>
    val wasEmpty = nonEmpty(str, context).headOption
    val wasUUID =
      if (uuidPattern.pattern.matcher(str).matches) {
        None
      } else {
        Some(
          Failure(s"Validation failure: $str not UUID", s"$context was not in the correct format", 400, Some(context))
        )
      }
    wasEmpty.orElse(wasUUID).toList
  }

  val positiveInteger: Validator[Int] = { (i, context) =>
    if (i < 0) {
      List(
        Failure("Validation failure: empty", s"$context must be a positive number", 400, Some(context))
      )
    } else Nil
  }

  val greaterThanZero: Validator[Int] = { (i, context) =>
    if (i <= 0) {
      List(
        Failure("Validation failure: empty", s"$context must be a positive number", 400, Some(context))
      )
    } else Nil
  }

  /**
   * Game codes are a case-insensitive UUID prefix
   */
  val gameCode: Validator[String] = { (str, context) =>
    val wasTooShort = minLength(4)(str, context).headOption
    val ValidChar = "([0-9a-fA-F\\-])".r
    val valid = str.zipWithIndex.forall {
      case (ValidChar(c), i) =>
        if (i == 8 || i == 13 || i == 18 || i == 23) {
          c == '-'
        } else true
      case _ =>
        false
    }
    val wasUUIDPrefix =
      if (valid) None
      else Some(Failure(s"$str is not a UUID prefix", "Invalid game code", 400, Some(context)))
    wasTooShort.orElse(wasUUIDPrefix).toList
  }

  def minLength(min: Int): Validator[String] = { (str, context) =>
    if (str.length < min)
      List(
        Failure("Failed min length", s"$context must be at least $min characters", 400, Some(context))
      )
    else Nil
  }

  def maxLength(max: Int): Validator[String] = { (str, context) =>
    if (str.length > max)
      List(
        Failure("Failed max length", s"$context must be no more than $max characters", 400, Some(context))
      )
    else Nil
  }

  def sensibleLength: Validator[String] = { (str, context) =>
    nonEmpty(str, context) ++ maxLength(50)(str, context)
  }
}
