package org.fathens.moon

import java.util.Date

import org.scalacheck._

object AstronomicSpec extends SpecificationExt {
  def is = s2"""

  Julian Days Number

    Epoch 1980-01-01 => 1                           $ed01
    Examples around 1970-01-01                      $ed02
    Considering hours                               $ed03
  
  Kepler's equation

  Example of solutions (at 2015-01-01 - 2015-01-31) $kepler
  """

  implicit val precision = Precision(5e-6)

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
        (x, y) => Astronomic.kepler(x.toRadians, Astronomic.eccentricity) must_=~ y
      }
}
