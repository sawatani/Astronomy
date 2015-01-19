package org.fathens.moon

import org.specs2.Specification

object AstronomicSpec extends Specification {
  def is = s2"""

  Kepler's equation

  Example of solutions (at 2015-01-01 - 2015-01-31)
    ex01                                         $ex01
    ex02                                         $ex02
    ex03                                         $ex03
    ex04                                         $ex04
    ex05                                         $ex05
    ex06                                         $ex06
    ex07                                         $ex07
    ex08                                         $ex08
    ex09                                         $ex09
    ex10                                         $ex10
    ex11                                         $ex11
    ex12                                         $ex12
    ex13                                         $ex13
    ex14                                         $ex14
    ex15                                         $ex15
    ex16                                         $ex16
    ex17                                         $ex17
    ex18                                         $ex18
    ex19                                         $ex19
    ex20                                         $ex20
    ex21                                         $ex21
    ex22                                         $ex22
    ex23                                         $ex23
    ex24                                         $ex24
    ex25                                         $ex25
    ex26                                         $ex26
    ex27                                         $ex27
    ex28                                         $ex28
    ex29                                         $ex29
    ex30                                         $ex30
    ex31                                         $ex31
  """

  def kepler(x: Double, y: Double) = {
    Astronomic.kepler(x, Astronomic.eccentricity) must beCloseTo(y, 5e-6)
  }
  def ex01 = kepler(358.23110, 6.25179)
  def ex02 = kepler(359.21675, 6.26928)
  def ex03 = kepler(0.20240, 0.00359)
  def ex04 = kepler(1.18804, 0.02109)
  def ex05 = kepler(2.17369, 0.03858)
  def ex06 = kepler(3.15934, 0.05608)
  def ex07 = kepler(4.14499, 0.07357)
  def ex08 = kepler(5.13063, 0.09107)
  def ex09 = kepler(6.11628, 0.10856)
  def ex10 = kepler(7.10193, 0.12605)
  def ex11 = kepler(8.08757, 0.14355)
  def ex12 = kepler(9.07322, 0.16104)
  def ex13 = kepler(10.05887, 0.17853)
  def ex14 = kepler(11.04452, 0.19602)
  def ex15 = kepler(12.03016, 0.21351)
  def ex16 = kepler(13.01581, 0.23100)
  def ex17 = kepler(14.00146, 0.24848)
  def ex18 = kepler(14.98711, 0.26597)
  def ex19 = kepler(15.97275, 0.28345)
  def ex20 = kepler(16.95840, 0.30094)
  def ex21 = kepler(17.94405, 0.31842)
  def ex22 = kepler(18.92970, 0.33590)
  def ex23 = kepler(19.91534, 0.35337)
  def ex24 = kepler(20.90099, 0.37085)
  def ex25 = kepler(21.88664, 0.38832)
  def ex26 = kepler(22.87228, 0.40580)
  def ex27 = kepler(23.85793, 0.42327)
  def ex28 = kepler(24.84358, 0.44073)
  def ex29 = kepler(25.82923, 0.45820)
  def ex30 = kepler(26.81487, 0.47566)
  def ex31 = kepler(27.80052, 0.49312)
}
