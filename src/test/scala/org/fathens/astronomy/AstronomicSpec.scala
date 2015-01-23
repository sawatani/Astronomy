package org.fathens.astronomy

import java.util.Date

import org.scalacheck._
import org.specs2._
import org.specs2.matcher._
import org.fathens.math._

object AstronomicSpec extends Specification with DataTables with ScalaCheck {
  def is = s2"""

  Julian Days Number

    Epoch 1980-01-01 => 1                           $ed01
    Examples around 1970-01-01                      $ed02
    Considering hours                               $ed03
  
  Kepler's equation

  Example of solutions (at 2015-01-01 - 2015-01-31) $kepler
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
  
  def ed01 = Astronomic.Days.from1980(Astronomic.Days.iso8601 parse "1980-01-01T00:00:00.000Z") must_== 1
  def ed02 =
    "date" | "jdn" |
      2440587.50 ! "1970-01-01T00:00:00.000Z" |
      2440587.75 ! "1970-01-01T06:00:00.000Z" |
      2440588.00 ! "1970-01-01T12:00:00.000Z" |
      2440588.25 ! "1970-01-01T18:00:00.000Z" |
      2440588.50 ! "1970-01-02T00:00:00.000Z" |
      2440588.75 ! "1970-01-02T06:00:00.000Z" |
      2440589.00 ! "1970-01-02T12:00:00.000Z" |> { (jdn, date) =>
        Astronomic.Days.jdn(Astronomic.Days.iso8601 parse date) must_== jdn
      }
  def ed03 = Prop.forAll(genDate, Gen.choose(0, 24 * 60 * 60)) { (date, seconds) =>
    val jdn = Astronomic.Days jdn new Date(_: Long)
    jdn(date.getTime + seconds * 1000) must_=~ jdn(date.getTime) + seconds / (24 * 60 * 60.0)
  }.set(minTestsOk = 10000)

  def kepler =
    "x" | "y" |
      358.23110 ! 6.25179 |
      359.21675 ! 6.26928 |
      0.20240 ! 3.59261e-3 |
      1.18804 ! 2.10877e-2 |
      2.17369 ! 3.85829e-2 |
      3.15934 ! 5.60779e-2 |
      4.14499 ! 7.35726e-2 |
      5.13063 ! 9.10667e-2 |
      6.11628 ! 1.08561e-1 |
      7.10193 ! 1.26054e-1 |
      8.08757 ! 1.43546e-1 |
      9.07322 ! 1.61038e-1 |
      10.05887 ! 1.78529e-1 |
      11.04452 ! 1.96019e-1 |
      12.03016 ! 2.13508e-1 |
      13.01581 ! 2.30996e-1 |
      14.00146 ! 2.48483e-1 |
      14.98711 ! 2.65969e-1 |
      15.97275 ! 2.83453e-1 |
      16.95840 ! 3.00935e-1 |
      17.94405 ! 3.18417e-1 |
      18.92970 ! 3.35896e-1 |
      19.91534 ! 3.53374e-1 |
      20.90099 ! 3.70850e-1 |
      21.88664 ! 3.88324e-1 |
      22.87228 ! 4.05796e-1 |
      23.85793 ! 4.23266e-1 |
      24.84358 ! 4.40734e-1 |
      25.82923 ! 4.58200e-1 |
      26.81487 ! 4.75663e-1 |
      27.80052 ! 4.93125e-1 |> {
        (x, y) => Astronomic.kepler(Degrees(x), Astronomic.eccentricity).value must_=~ y
      }
}
