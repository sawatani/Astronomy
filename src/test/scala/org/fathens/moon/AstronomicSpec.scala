package org.fathens.moon

import org.specs2._
import org.scalacheck._

object AstronomicSpec extends Specification with matcher.DataTables with ScalaCheck {
  def is = s2"""

  Epoch days                                        $ed01
  
  Fixangle (normalize degrees)

  always be in 0 - 360                              $fa01
  0-359    => it self                               $fa02
  over 360 => decrease 360                          $fa03
  under 0  => increase 360                          $fa04

  Kepler's equation

  Example of solutions (at 2015-01-01 - 2015-01-31) $kepler
  """

  implicit val precision = 5e-6
  implicit class moreDouble(a: Double) {
    def must_=~(b: Double)(implicit p: Double) = a must beCloseTo(b, p)
  }
  
  def ed01 = {
    val date = Astronomic.Days.iso8601 parse "1980-01-01T00:00:00.000Z"
    (Astronomic.Days from1980 date) must_== 1.5
  }
  def fa01 = prop { (d: Double) =>
    Astronomic.fixangle(d) must beBetween(0.0, 360.0).excludingEnd
  }
  def fa02 = Prop.forAll(Gen.choose(0.0, 359.9)) { (d: Double) =>
    Astronomic.fixangle(d) must_=~ d
  }
  def fa03 = Prop.forAll(Gen.choose(0.0, 359.9)) { (d: Double) =>
    Astronomic.fixangle(360 + d) must_=~ d
  }
  def fa04 = Prop.forAll(Gen.choose(0.0, 359.9)) { (d: Double) =>
    Astronomic.fixangle(d - 360) must_=~ d
  }
  def kepler =
    "x" | "y" |
      358.23110 ! 6.25179 |
      359.21675 ! 6.26928 |
      0.20240 ! 0.00359 |
      1.18804 ! 0.02109 |
      2.17369 ! 0.03858 |
      3.15934 ! 0.05608 |
      4.14499 ! 0.07357 |
      5.13063 ! 0.09107 |
      6.11628 ! 0.10856 |
      7.10193 ! 0.12605 |
      8.08757 ! 0.14355 |
      9.07322 ! 0.16104 |
      10.05887 ! 0.17853 |
      11.04452 ! 0.19602 |
      12.03016 ! 0.21351 |
      13.01581 ! 0.23100 |
      14.00146 ! 0.24848 |
      14.98711 ! 0.26597 |
      15.97275 ! 0.28345 |
      16.95840 ! 0.30094 |
      17.94405 ! 0.31842 |
      18.92970 ! 0.33590 |
      19.91534 ! 0.35337 |
      20.90099 ! 0.37085 |
      21.88664 ! 0.38832 |
      22.87228 ! 0.40580 |
      23.85793 ! 0.42327 |
      24.84358 ! 0.44073 |
      25.82923 ! 0.45820 |
      26.81487 ! 0.47566 |
      27.80052 ! 0.49312 |> {
        (x, y) => Astronomic.kepler(x, Astronomic.eccentricity) must_=~ y
      }
}
