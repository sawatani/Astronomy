package org.fathens.astronomy

import org.fathens.math._

object Sun {
  /**
   * Ecliptic longitude of the Sun at epoch 1980.0
   */
  val ecliptic_longitude_epoch = Degrees(278.83354)
  /**
   * Ecliptic longitude of the Sun at perigee
   */
  val ecliptic_longitude_perigee = Degrees(282.596403)
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

}
class Sun(val date: java.util.Date) {
  import Sun._

  lazy val mean_anomaly_perigee = {
    val day = Days from1980 date
    val N = day * Pi2 / Days.one_year
    // Convert from perigee coordinates to epoch 1980
    ecliptic_longitude_epoch + N - ecliptic_longitude_perigee
  }
  lazy val true_anomaly = {
    // Eccentric anomaly
    val a = Equations.kepler(mean_anomaly_perigee, eccentricity)
    val b = ((1 + eccentricity) / (1 - eccentricity)).sqrt * tan(a / 2.0)
    2 * atan(b)
  }

  lazy val (distance, angular_diameter) = {
    // Orbital distance factor
    val F = (1 + eccentricity * cos(true_anomaly)) / (1 - (eccentricity ^ 2))
    (sun_smaxis / F, F * sun_angular_size_smaxis)
  }

  /**
   * Geocentric ecliptic longitude
   */
  lazy val ecliptic_longitude = ecliptic_longitude_perigee + true_anomaly 
}
