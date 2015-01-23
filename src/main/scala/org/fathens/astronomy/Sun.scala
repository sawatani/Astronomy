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
class Sun(date: java.util.Date) {
  import Sun._

  val mean_anomaly_perigee = {
    val day = Days from1980 date
    val N = day * Pi2 / Days.one_year
    // Convert from perigee coordinates to epoch 1980
    N + ecliptic_longitude_epoch - ecliptic_longitude_perigee
  }
  val true_anomaly = {
    // Eccentric anomaly
    val a = Equations.kepler(mean_anomaly_perigee, eccentricity)
    val b = ((1 + eccentricity) / (1 - eccentricity)).sqrt * tan(a / 2.0)
    2 * atan(b)
  }

  val (distance, angular_diameter) = {
    // Orbital distance factor
    val F = (1 + eccentricity * cos(true_anomaly)) / (1 - (eccentricity ^ 2))
    (sun_smaxis / F, F * sun_angular_size_smaxis)
  }

  /**
   * Geocentric ecliptic longitude
   */
  val ecliptic_longitude = true_anomaly + ecliptic_longitude_perigee
}
