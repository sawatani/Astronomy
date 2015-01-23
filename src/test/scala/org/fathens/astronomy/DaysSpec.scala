package org.fathens.astronomy

import java.util.Date

import org.scalacheck._
import org.specs2._
import org.specs2.matcher._
import org.fathens.math._

object DaysSpec extends Specification with DataTables with ScalaCheck {
  def is = s2"""

  Julian Days Number

    Epoch 1980-01-01 => 1                           $ed01
    Examples around 1970-01-01                      $ed02
    Considering hours                               $ed03
  """

  /**
   * Extend Double Matchers
   */
  implicit class MoreDouble(a: Double) {
    def must_=~(b: Double)(implicit p: SignificantFigures) = a must beCloseTo(b, p)
  }
  implicit val precision = SignificantFigures(6)
  /**
   * Date Generator
   */
  val genDate = Gen.choose(0, new java.util.Date().getTime * 2).map(new java.util.Date(_))
  
  def ed01 = Days.from1980(Days.iso8601 parse "1980-01-01T00:00:00.000Z") must_== 1
  def ed02 =
    "date" | "jdn" |
      2440587.50 ! "1970-01-01T00:00:00.000Z" |
      2440587.75 ! "1970-01-01T06:00:00.000Z" |
      2440588.00 ! "1970-01-01T12:00:00.000Z" |
      2440588.25 ! "1970-01-01T18:00:00.000Z" |
      2440588.50 ! "1970-01-02T00:00:00.000Z" |
      2440588.75 ! "1970-01-02T06:00:00.000Z" |
      2440589.00 ! "1970-01-02T12:00:00.000Z" |> { (jdn, date) =>
        Days.jdn(Days.iso8601 parse date) must_== jdn
      }
  def ed03 = Prop.forAll(genDate, Gen.choose(0, 24 * 60 * 60)) { (date, seconds) =>
    val jdn = Days jdn new Date(_: Long)
    jdn(date.getTime + seconds * 1000) must_=~ jdn(date.getTime) + seconds / (24 * 60 * 60.0)
  }.set(minTestsOk = 10000)
}
