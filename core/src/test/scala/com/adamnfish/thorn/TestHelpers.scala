package com.adamnfish.thorn

import com.adamnfish.thorn.models.{Disc, Rose, Thorn}
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
    Gen.oneOf(Thorn, Rose)

  implicit val arbDisc: Arbitrary[Disc] =
    Arbitrary(discGen)
}
