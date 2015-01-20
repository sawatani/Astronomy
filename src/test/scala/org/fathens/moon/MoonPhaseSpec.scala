package org.fathens.moon

import java.util.Date

import org.scalacheck._

object MoonPhaseSpec extends SpecificationExt {
  def is = s2"""
  Examples of moon phase (at 2015-01-01 - 2015-01-31)    $phases
    
  Circle (normalize radians)

    always inside 0 - 2π                                 $fa01
    0-2π    => it self                                   $fa02
    over 2π => decrease 2π            　                 $fa03
    under 0 => increase 2π              　               $fa04

  Illuminated is depend on phase                         $ip
"""

  implicit val precision = Precision(5e-6)

  def fa01 = prop { (d: Double) =>
    MoonPhase.circle(d) must beBetween(0.0, 2 * math.Pi).excludingEnd
  }
  def fa02 = Prop.forAll(Gen.choose(0.0, 2 * math.Pi - 0.00001)) { (d: Double) =>
    MoonPhase.circle(d) must_=~ d
  }
  def fa03 = Prop.forAll(Gen.choose(0.0, 2 * math.Pi - 0.00001)) { (d: Double) =>
    MoonPhase.circle(2 * math.Pi + d) must_=~ d
  }
  def fa04 = Prop.forAll(Gen.choose(0.0, 2 * math.Pi - 0.00001)) { (d: Double) =>
    MoonPhase.circle(d - 2 * math.Pi) must_=~ d
  }
  def ip = Prop.forAll(genDate) { (date: Date) =>
    val moon = new MoonPhase(date)
    moon.illuminated must_=~ (1 - math.cos(moon.phase * math.Pi * 2)) / 2
  }
  def phases =
    "age" | "date" |
      11.19911 ! "2015-01-01T00:00:00.000Z" |
      12.18579 ! "2015-01-02T00:00:00.000Z" |
      13.15564 ! "2015-01-03T00:00:00.000Z" |
      14.10875 ! "2015-01-04T00:00:00.000Z" |
      15.04567 ! "2015-01-05T00:00:00.000Z" |
      15.96750 ! "2015-01-06T00:00:00.000Z" |
      16.87600 ! "2015-01-07T00:00:00.000Z" |
      17.77363 ! "2015-01-08T00:00:00.000Z" |
      18.66360 ! "2015-01-09T00:00:00.000Z" |
      19.54998 ! "2015-01-10T00:00:00.000Z" |
      20.43772 ! "2015-01-11T00:00:00.000Z" |
      21.33269 ! "2015-01-12T00:00:00.000Z" |
      22.24158 ! "2015-01-13T00:00:00.000Z" |
      23.17149 ! "2015-01-14T00:00:00.000Z" |
      24.12945 ! "2015-01-15T00:00:00.000Z" |
      25.12148 ! "2015-01-16T00:00:00.000Z" |
      26.15154 ! "2015-01-17T00:00:00.000Z" |
      27.22036 ! "2015-01-18T00:00:00.000Z" |
      28.32458 ! "2015-01-19T00:00:00.000Z" |
      29.45650 ! "2015-01-20T00:00:00.000Z" |
      01.07424 ! "2015-01-21T00:00:00.000Z" |
      02.22573 ! "2015-01-22T00:00:00.000Z" |
      03.36752 ! "2015-01-23T00:00:00.000Z" |
      04.48915 ! "2015-01-24T00:00:00.000Z" |
      05.58386 ! "2015-01-25T00:00:00.000Z" |
      06.64879 ! "2015-01-26T00:00:00.000Z" |
      07.68417 ! "2015-01-27T00:00:00.000Z" |
      08.69224 ! "2015-01-28T00:00:00.000Z" |
      09.67613 ! "2015-01-29T00:00:00.000Z" |
      10.63912 ! "2015-01-30T00:00:00.000Z" |
      11.58429 ! "2015-01-31T00:00:00.000Z" |> {
        (age, date) => new MoonPhase(Astronomic.Days.iso8601 parse date).age must_=~ age
      }
}