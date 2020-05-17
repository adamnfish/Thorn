package com.adamnfish.thorn

import com.adamnfish.thorn.attempt.{Attempt, FailedAttempt}
import org.scalactic.source
import org.scalatest.EitherValues
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}


trait AttemptValues extends EitherValues with Matchers {
  implicit val ec: ExecutionContext = global

  implicit class RichAttempt[A](attempt: Attempt[A]) {
    def value()(implicit ec: ExecutionContext, pos: source.Position): A = {
      val result = Await.result(attempt.asFuture, 5.seconds)
      result.fold(
        { fa =>
          throw new TestFailedException(
            _ => Some(s"Expected successful attempt, got failure `${fa.logString}`"),
            None, pos
          )
        },
        identity
      )
    }

    def leftValue()(implicit ec: ExecutionContext, pos: source.Position): FailedAttempt = {
      val result = Await.result(attempt.asFuture, 5.seconds)
      result.fold(
        identity,
        { a =>
          throw new TestFailedException(
            _ => Some(s"Expected failed attempt, got success `$a`"),
            None, pos
          )
        }
      )
    }

    def isSuccessfulAttempt()(implicit ec: ExecutionContext, pos: source.Position): Unit = {
      val result = Await.result(attempt.asFuture, 5.seconds)
      result.fold(
        { fa =>
          throw new TestFailedException(
            _ => Some(s"Expected successful attempt, got failure `${fa.logString}`"),
            None, pos
          )
        },
        _ => ()
      )
    }

    def isFailedAttempt()(implicit ec: ExecutionContext, pos: source.Position): Unit = {
      val result = Await.result(attempt.asFuture, 5.seconds)
      result.fold(
        _ => (),
        { a =>
          throw new TestFailedException(
            _ => Some(s"Expected failed attempt but got success `$a`"),
            None, pos
          )
        }
      )
    }
  }
}
