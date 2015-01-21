package org.fathens.moon

import java.util.Date

import scala.annotation.tailrec
import scala.math._

object Astronomic {
  object Days {
    def iso8601 = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
    /**
     * 1970-01-01 in Astronomic Jujian Days Number
     */
    val JDN_1970_1_1 = 2440587.5
    /**
     * 1980-0-0 in Astronomic Jujian Days Number
     */
    val JDN_1980_0 = 2444238.5
    /**
     * Calculate Astronomic Jujian Days Number
     */
    def jdn(date: Date) = JDN_1970_1_1 + date.getTime / (24 * 60 * 60 * 1000.0) 
    /**
     * From 1980 January 0.0 in Astronomic Jujian Days Number
     */
    def from1980(date: Date) = jdn(date) - JDN_1980_0
  }
  val days_in_year = 365.2422
  /**
   * Ecliptic longitude of the Sun at epoch 1980.0 in radians
   */
  val ecliptic_longitude_epoch = 4.86656333799131
  /**
   * Ecliptic longitude of the Sun at perigee in radians
   */
  val ecliptic_longitude_perigee = 4.932237686642781
  /**
   * Eccentricity of Earth's orbit
   */
  val eccentricity = 0.016718
  /**
   * Semi-major axis of Earth's orbit, in kilometers
   */
  val sun_smaxis = 1.49585e8
  /**
   * Sun's angular size, in radians, at semi-major axis distance
   */
  val sun_angular_size_smaxis = 0.00930483893457233
  /**
   * (Elements of the Moon's orbit, epoch 1980.0)
   * Moon's mean longitude at the epoch in radians
   */
  val moon_mean_longitude_epoch = 1.134035779811045
  /**
   * (Elements of the Moon's orbit, epoch 1980.0)
   * Mean longitude of the perigee at the epoch in radians
   */
  val moon_mean_perigee_epoch = 6.097884800052777
  /**
   * Mean longitude of the node at the epoch in radians
   */
  val node_mean_longitude_epoch = 2.652035285867875
  /**
   * Inclination of the Moon's orbit in radians
   */
  val moon_inclination = 0.08980410151894615
  /**
   * Eccentricity of the Moon's orbit
   */
  val moon_eccentricity = 0.054900
  /**
   * Moon's angular size, in radians, at distance a from Earth
   */
  val moon_angular_size = 0.009042550854582622
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
   * Solve the equation of Kepler.
   */
  def kepler(m: Double, ecc: Double) = {
    val epsilon = 1e-6
    @tailrec
    def solve(e: Double): Double = {
      val delta = e - ecc * sin(e) - m
      val next = e - delta / (1.0 - ecc * cos(e))
      if (abs(delta) <= epsilon) next
      else solve(next)
    }
    solve(m)
  }
}
