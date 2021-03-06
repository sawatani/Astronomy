package org.fathens.astronomy

import java.util.Date

import org.scalacheck._
import org.specs2._
import org.specs2.matcher._
import org.fathens.math._

object MoonSpec extends Specification with DataTables with ScalaCheck {
  def is = s2"""
  Examples of moon phase (at 2015-01-01 - 2015-01-31)    $phases

  Illuminated is depend on phase                         $ip
"""

  /**
   * Extend Double Matchers
   */
  implicit class MoreDouble(a: Double) {
    def must_=~(b: Double)(implicit p: SignificantFigures) = a must beCloseTo(b, p)
  }
  implicit val precision = SignificantFigures(5)
  /**
   * Date Generator
   */
  val genDate = Gen.choose(0, new java.util.Date().getTime * 2).map(new java.util.Date(_))

  def ip = Prop.forAll(genDate) { (date: Date) =>
    val moon = new Moon(date)
    moon.illuminated must_=~ (1 - math.cos(moon.phase * math.Pi * 2)) / 2
  }
  def phases =
    "age" | "date" |
      10.69947 ! "2015-01-01T00:00:00.000Z" |
      11.69455 ! "2015-01-02T00:00:00.000Z" |
      12.67282 ! "2015-01-03T00:00:00.000Z" |
      13.63427 ! "2015-01-04T00:00:00.000Z" |
      14.57918 ! "2015-01-05T00:00:00.000Z" |
      15.50838 ! "2015-01-06T00:00:00.000Z" |
      16.42328 ! "2015-01-07T00:00:00.000Z" |
      17.32599 ! "2015-01-08T00:00:00.000Z" |
      18.21934 ! "2015-01-09T00:00:00.000Z" |
      19.10695 ! "2015-01-10T00:00:00.000Z" |
      19.99333 ! "2015-01-11T00:00:00.000Z" |
      20.88390 ! "2015-01-12T00:00:00.000Z" |
      21.78496 ! "2015-01-13T00:00:00.000Z" |
      22.70346 ! "2015-01-14T00:00:00.000Z" |
      23.64656 ! "2015-01-15T00:00:00.000Z" |
      24.62090 ! "2015-01-16T00:00:00.000Z" |
      25.63162 ! "2015-01-17T00:00:00.000Z" |
      26.68121 ! "2015-01-18T00:00:00.000Z" |
      27.76843 ! "2015-01-19T00:00:00.000Z" |
      28.88772 ! "2015-01-20T00:00:00.000Z" |
      4.98833e-1 ! "2015-01-21T00:00:00.000Z" |
      01.65043 ! "2015-01-22T00:00:00.000Z" |
      02.79857 ! "2015-01-23T00:00:00.000Z" |
      03.93137 ! "2015-01-24T00:00:00.000Z" |
      05.04013 ! "2015-01-25T00:00:00.000Z" |
      06.12009 ! "2015-01-26T00:00:00.000Z" |
      07.17006 ! "2015-01-27T00:00:00.000Z" |
      08.19144 ! "2015-01-28T00:00:00.000Z" |
      09.18700 ! "2015-01-29T00:00:00.000Z" |
      10.16004 ! "2015-01-30T00:00:00.000Z" |
      11.11375 ! "2015-01-31T00:00:00.000Z" |> { (age, date) =>
        new Moon(Days.iso8601 parse date).age must_=~ age
      }
}