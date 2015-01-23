package org.fathens.moon

import java.util.Date

import scala.annotation.tailrec
import org.fathens.math._

object Astronomic {
  object Days {
    def iso8601 = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
    /**
     * Calculate Astronomic Jujian Days Number
     */
    def jdn(date: Date) = 2440587.5 + date.getTime / (24 * 60 * 60 * 1000.0)
    /**
     * From 1980 January 0.0 in Astronomic Jujian Days Number
     */
    def from1980(date: Date) = jdn(date) - 2444238.5
  }
  val days_in_year = 365.2422
  /**
   * Ecliptic longitude of the Sun at epoch 1980.0 in radians
   */
  val ecliptic_longitude_epoch = Radians(4.86656333799131)
  /**
   * Ecliptic longitude of the Sun at perigee in radians
   */
  val ecliptic_longitude_perigee = Radians(4.932237686642781)
  /**
   * Eccentricity of Earth's orbit
   */
  val eccentricity = 0.016718
  /**
   * Semi-major axis of Earth's orbit, in kilometers
   */
  val sun_smaxis = Killometers(1.49585e8)
  /**
   * Sun's angular size, in radians, at semi-major axis distance
   */
  val sun_angular_size_smaxis = Radians(0.00930483893457233)
  /**
   * (Elements of the Moon's orbit, epoch 1980.0)
   * Moon's mean longitude at the epoch in radians
   */
  val moon_mean_longitude_epoch = Radians(1.134035779811045)
  /**
   * (Elements of the Moon's orbit, epoch 1980.0)
   * Mean longitude of the perigee at the epoch in radians
   */
  val moon_mean_perigee_epoch = Radians(6.097884800052777)
  /**
   * Mean longitude of the node at the epoch in radians
   */
  val node_mean_longitude_epoch = Radians(2.652035285867875)
  /**
   * Inclination of the Moon's orbit in radians
   */
  val moon_inclination = Radians(0.08980410151894615)
  /**
   * Eccentricity of the Moon's orbit
   */
  val moon_eccentricity = 0.054900
  /**
   * Moon's angular size, in radians, at distance a from Earth
   */
  val moon_angular_size = Radians(0.009042550854582622)
  /**
   * Semi-mojor axis of the Moon's orbit, in kilometers
   */
  val moon_smaxis = Killometers(384401.0)
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
  val earth_radius = Killometers(6378.16)
  /**
   * Solve the equation of Kepler.
   */
  def kepler(m: Radians, ecc: Double) = {
    val epsilon = 1e-6
    @tailrec
    def solve(e: Radians): Radians = {
      val delta = e - m - Radians(ecc * sin(e))
      val next = e - delta / (1.0 - ecc * cos(e))
      if (delta.abs.value <= epsilon) next
      else solve(next)
    }
    solve(m)
  }
}
