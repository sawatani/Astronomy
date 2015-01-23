package org.fathens.astronomy

import java.util.Date

import org.fathens.math._

import com.typesafe.scalalogging.LazyLogging

object Moon extends LazyLogging {
  /**
   * (Elements of the Moon's orbit, epoch 1980.0)
   * Moon's mean longitude at the epoch
   */
  val mean_longitude_epoch = Degrees(64.975464)
  /**
   * (Elements of the Moon's orbit, epoch 1980.0)
   * Mean longitude of the perigee at the epoch
   */
  val mean_perigee_epoch = Degrees(349.383063)

  /**
   * Inclination of the Moon's orbit
   */
  val inclination = Degrees(5.145396)
  /**
   * Eccentricity of the Moon's orbit
   */
  val eccentricity = 0.054900
  /**
   * Moon's angular size, in radians, at distance a from Earth
   */
  val angular_size = Radians(0.009042550854582622)
  /**
   * Semi-mojor axis of the Moon's orbit, in kilometers
   */
  val smaxis = Killometers(384401.0)
  /**
   * Synodic month (new Moon to new Moon), in days
   */
  val synodic_month = 29.53058868
  /**
   * Mean longitude of the node at the epoch
   */
  val node_mean_longitude_epoch = Degrees(151.950429)
}
class Moon(date: Date) extends LazyLogging {
  import Moon._

  lazy val (rotation, true_anomaly) = {
    val sun = Sun(date)
    val day = Days from1980 date

    // Moon's mean longitude
    val moon_longitude = Degrees(13.1763966) * day + mean_longitude_epoch

    // Moon's mean anomaly
    val MM = moon_longitude - Degrees(0.1114041) * day - mean_perigee_epoch

    // Moon's ascending node mean longitude
    val evection = Degrees(1.2739) * sin(2 * (moon_longitude - sun.ecliptic_longitude) - MM)

    // Annual equation
    val annual_eq = Degrees(0.1858) * sin(sun.mean_anomaly_perigee)

    val MmP = MM + evection - annual_eq - Degrees(0.37) * sin(sun.mean_anomaly_perigee)

    // Correction for the equation of the centre
    val mEc = Degrees(6.2886) * sin(MmP)

    // True longitude
    val lPP = {
      // Corrected longitude
      val lP = moon_longitude + evection + mEc - annual_eq + Degrees(0.214) * sin(2 * MmP)
      // Variation
      val variation = Degrees(0.6583) * sin(2 * (lP - sun.ecliptic_longitude))

      lP + variation
    }
    logger trace f"lPP=${lPP}"

    ///////
    //
    // Calculation of the Moon's inclination
    // unused for phase calculation.
    val inclinations = new {
      // Corrected longitude of the node
      private val NP = {
        // Moon's ascending node mean longitude
        val MN = node_mean_longitude_epoch - Degrees(0.0529539) * day
        MN - Degrees(0.16) * sin(sun.mean_anomaly_perigee)
      }

      // Ecliptic longitude
      val lambda_moon = {
        // Y inclination coordinate
        val y = sin(lPP - NP) * cos(inclination)
        // X inclination coordinate
        val x = cos(lPP - NP)

        atan2(y, x) + NP
      }

      // Ecliptic latitude
      val BetaM = asin(sin(lPP - NP) * sin(inclination))
    }
    logger debug f"inclination: lambda_moon: ${inclinations.lambda_moon}, BetaM: ${inclinations.BetaM}"

    (lPP - sun.ecliptic_longitude, MmP + mEc)
  }
  /**
   * The terminator phase angle as a percentage of a full circle (i.e., 0 to 1)
   */
  lazy val phase = rotation.normalize / Pi2
  /**
   * The illuminated fraction of the Moon's disc
   */
  lazy val illuminated = (1 - cos(rotation)) / 2.0
  /**
   * Age of the Moon, in degrees
   */
  lazy val age = synodic_month * phase
  /**
   * distance of Moon from the centre of the Earth
   */
  lazy val distance = smaxis * (1 - (eccentricity ^ 2)) / (1 + eccentricity * cos(true_anomaly))
  /**
   * Moon's angular diameter
   */
  lazy val angularDiameter = smaxis / distance * angular_size

  override def toString = {
    f"MoonPhase(illuminated: ${illuminated * 100}%3.5f%%, age: ${age}%02.5f)"
  }
}
