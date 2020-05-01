package com.adamnfish.skull

import com.adamnfish.skull.models.{Disc, Rose, Skull}
import org.scalacheck.{Arbitrary, Gen}
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

  implicit val discGen: Gen[Disc] =
    Gen.oneOf(Skull, Rose)

  implicit val arbDisc: Arbitrary[Disc] =
    Arbitrary(discGen)
}
