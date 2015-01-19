package org.fathens.moon

import java.util.Date

import scala.annotation.tailrec
import scala.math._

object Astronomic {
  object Days {
    val parse = java.time.OffsetDateTime.parse(_: String)
    val jdn = java.time.temporal.JulianFields.JULIAN_DAY.getFrom(_: java.time.OffsetDateTime)
    val epoch = 2444238.5
    val iso8601 = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
    /**
     * From 1980 January 0.0 in JDN
     */
    def from1980(date: Date) = jdn(parse(iso8601 format date)) - epoch
  }
  /**
   * Ecliptic longitude of the Sun at epoch 1980.0
   */
  val ecliptic_longitude_epoch = 278.833540
  /**
   * Ecliptic longitude of the Sun at perigee
   */
  val ecliptic_longitude_perigee = 282.596403
  /**
   * Eccentricity of Earth's orbit
   */
  val eccentricity = 0.016718
  /**
   * Semi-major axis of Earth's orbit, in kilometers
   */
  val sun_smaxis = 1.49585e8
  /**
   * Sun's angular size, in degrees, at semi-major axis distance
   */
  val sun_angular_size_smaxis = 0.533128
  /**
   * (Elements of the Moon's orbit, epoch 1980.0)
   * Moon's mean longitude at the epoch
   */
  val moon_mean_longitude_epoch = 64.975464
  /**
   * (Elements of the Moon's orbit, epoch 1980.0)
   * Mean longitude of the perigee at the epoch
   */
  val moon_mean_perigee_epoch = 349.383063
  /**
   * Mean longitude of the node at the epoch
   */
  val node_mean_longitude_epoch = 151.950429
  /**
   * Inclination of the Moon's orbit
   */
  val moon_inclination = 5.145396
  /**
   * Eccentricity of the Moon's orbit
   */
  val moon_eccentricity = 0.054900
  /**
   * Moon's angular size at distance a from Earth
   */
  val moon_angular_size = 0.5181
  /**
   * Semi-mojor axis of the Moon's orbit, in kilometers
   */
  val moon_smaxis = 384401.0
  /**
   * Synodic month (new Moon to new Moon), in days
   */
  val synodic_month = 29.53058868
  /**
   * Base date for E. W. Brown's numbered series of lunations (1923 January 16)
   */
  val lunations_base = 2423436.0
  /**
   * Properties of the Earth
   */
  val earth_radius = 6378.16
  /**
   * Normalize degrees to (0 <= d < 360)
   */
  def fixangle(a: Double) = (360 + a % 360) % 360
  /**
   * Solve the equation of Kepler.
   */
  def kepler(m: Double, ecc: Double) = {
    val initial = m.toRadians
    val epsilon = 1e-6
    @tailrec
    def solve(e: Double): Double = {
      val delta = e - ecc * sin(e) - initial
      val next = e - delta / (1.0 - ecc * cos(e))
      if (abs(delta) <= epsilon) next
      else solve(next)
    }
    solve(initial)
  }
}
