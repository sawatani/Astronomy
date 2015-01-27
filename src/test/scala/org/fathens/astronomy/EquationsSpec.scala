package org.fathens.astronomy

import org.scalacheck._
import org.specs2._
import org.specs2.matcher._
import org.fathens.math._

object EquationsSpec extends Specification with DataTables with ScalaCheck {
  def is = s2"""

  Kepler's equation

    Example of solutions (at 2015-01-01 - 2015-01-31) $kp01
  """

  /**
   * Extend Double Matchers
   */
  implicit class MoreDouble(a: Double) {
    def must_=~(b: Double)(implicit p: SignificantFigures) = a must beCloseTo(b, p)
  }
  implicit val precision = SignificantFigures(6)

  def kp01 =
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
        (x, y) => Equations.kepler(Degrees(x), Sun.eccentricity).toDouble must_=~ y
      }
}