package org.fathens

import org.fathens.math._

package object astronomy {
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
  /**
   * (Elements of the Moon's orbit, epoch 1980.0)
   * Moon's mean longitude at the epoch
   */
  val moon_mean_longitude_epoch = Degrees(64.975464)
  /**
   * (Elements of the Moon's orbit, epoch 1980.0)
   * Mean longitude of the perigee at the epoch
   */
  val moon_mean_perigee_epoch = Degrees(349.383063)
  /**
   * Mean longitude of the node at the epoch
   */
  val node_mean_longitude_epoch = Degrees(151.950429)
  /**
   * Inclination of the Moon's orbit
   */
  val moon_inclination = Degrees(5.145396)
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
}
