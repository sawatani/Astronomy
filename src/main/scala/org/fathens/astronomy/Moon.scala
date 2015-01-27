package org.fathens.astronomy

import org.fathens.math._

object Moon {
  /**
   * Synodic month (new Moon to new Moon), in days
   */
  val synodic_month = 29.53058868
  /**
   * Orbital period of Moon
   */
  val orbital_days = 27.321582

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
   * Mean longitude of the node at the epoch
   */
  val node_mean_longitude_epoch = Degrees(151.950429)

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
}
class Moon(val date: java.util.Date) {
  import Moon._

  lazy val sun = new Sun(date)

  lazy val days_from_epoch = Days from1980 date
  /**
   * Mean longitude
   */
  lazy val mean_longitude = mean_longitude_epoch + Pi2 / orbital_days * days_from_epoch
  /**
   * Mean anomaly
   */
  lazy val mean_anomaly = mean_longitude - Degrees(0.1114041) * days_from_epoch - mean_perigee_epoch

  lazy val (true_longitude, true_anomaly) = {
    val ml = {
      val evection = Degrees(1.2739) * sin(2 * (mean_longitude - sun.ecliptic_longitude) - mean_anomaly)
      // Annual equation
      evection - Degrees(0.1858) * sin(sun.mean_anomaly_perigee)
    }
    val mp = mean_anomaly + ml - Degrees(0.37) * sin(sun.mean_anomaly_perigee)
    // Moon's equation of the center
    val ec = Degrees(6.2886) * sin(mp)

    // Corrected longitude
    val corrected_longitude = mean_longitude + ml + Degrees(0.214) * sin(2 * mp) + ec
    val variation = Degrees(0.6583) * sin(2 * (corrected_longitude - sun.ecliptic_longitude))

    (corrected_longitude + variation, mp + ec)
  }

  lazy val (ecliptic_latitude, ecliptic_longitude) = {
    // Corrected longitude of the node
    val np = {
      // Moon's ascending node mean longitude
      val mn = node_mean_longitude_epoch - Degrees(0.0529539) * days_from_epoch
      mn - Degrees(0.16) * sin(sun.mean_anomaly_perigee)
    }
    (
      asin(sin(true_longitude - np) * sin(inclination)), // Ecliptic Latitude
      atan2(sin(true_longitude - np) * cos(inclination), cos(true_longitude - np)) + np // Ecliptic Longitude
    )
  }

  /**
   * Angular of Sun and Moon 
   */
  lazy val angular_from_sun = true_longitude - sun.ecliptic_longitude
  /**
   * The terminator phase angle as a percentage of a full circle (i.e., 0 to 1)
   */
  lazy val phase = angular_from_sun.normalize / Pi2
  /**
   * The illuminated fraction of the Moon's disc
   */
  lazy val illuminated = (1 - cos(angular_from_sun)) / 2.0
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
  lazy val angular_diameter = smaxis / distance * angular_size

  /**
   * Longitude of earth
   */
  lazy val earth_longitude: Degrees = (angular_from_sun - Pi2 * (Days.jdn(date) % 1)).normalize

  override def toString = {
    f"MoonPhase(illuminated: ${illuminated * 100}%3.5f%%, age: ${age}%02.5f)"
  }
}
