package com.adamnfish.thorn

import com.adamnfish.thorn.models.{Disc, Rose, Thorn}
import org.scalacheck.{Arbitrary, Gen}
import org.scalactic.source
import org.scalatest.matchers.HavePropertyMatcher
import org.scalatest.matchers.should.Matchers


trait TestHelpers extends Matchers {
  def having[A](propertyName: String, propertyValue: A): HavePropertyMatcher[AnyRef, Any] = {
    Symbol(propertyName) (propertyValue)
  }

  implicit class HavingTestHelperString(propertyName: String) {
    def as[A](propertyValue: A): HavePropertyMatcher[AnyRef, Any] = {
      Symbol(propertyName) (propertyValue)
    }
  }

  def runMultiple[A](times: Int)(f: Unit => Either[String, A]): Either[List[String], List[A]] = {
    val results = (1 to times).map(_ => f(())).toList
    val failures = results.collect {
      case Left(msg) => msg
    }
    val successes = results.collect {
      case Right(a) =>
        a
    }

    if (failures.nonEmpty) {
      Left(failures)
    } else {
      Right(successes)
    }
  }

  implicit val discGen: Gen[Disc] =
    Gen.oneOf(Thorn, Rose)

  implicit val arbDisc: Arbitrary[Disc] =
    Arbitrary(discGen)
}
