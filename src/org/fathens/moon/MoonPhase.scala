package org.fathens.moon

import java.util.Date

import scala.math._

object MoonPhase {
  import Astronomic._
  def at(date: Date) = {
    val day = Days from1980 date
    // Mean anomaly of the Sun
    val N = fixangle((360 / 365.2422) * day)
    // Convert from perigee coordinates to epoch 1980
    val M = fixangle(N + ecliptic_longitude_epoch - ecliptic_longitude_perigee)

    // True anomaly
    val Ec = {
      // Solve Kepler's equation
      val a = kepler(M, eccentricity)
      val b = sqrt((1 + eccentricity) / (1 - eccentricity)) * tan(a / 2.0)
      2 * todeg(atan(b))
    }
    // Suns's geometric ecliptic longuitude
    val lambda_sun = fixangle(Ec + ecliptic_longitude_perigee)

    // Orbital distance factor
    val F = ((1 + eccentricity * cos(torad(Ec))) / (1 - pow(eccentricity, 2)))

    // Distance to Sun in km
    val sun_dist = sun_smaxis / F
    val sun_angular_diameter = F * sun_angular_size_smaxis

    ////////
    //
    // Calculation of the Moon's position

    // Moon's mean longitude
    val moon_longitude = fixangle(13.1763966 * day + moon_mean_longitude_epoch)

    // Moon's mean anomaly
    val MM = fixangle(moon_longitude - 0.1114041 * day - moon_mean_perigee_epoch)

    // Moon's ascending node mean longitude
    // MN = fixangle(c.node_mean_longitude_epoch - 0.0529539 * day)
    val evection = 1.2739 * sin(torad(2 * (moon_longitude - lambda_sun) - MM))

    // Annual equation
    val annual_eq = 0.1858 * sin(torad(M))

    // Correction term
    val A3 = 0.37 * sin(torad(M))

    val MmP = MM + evection - annual_eq - A3

    // Correction for the equation of the centre
    val mEc = 6.2886 * sin(torad(MmP))

    // Another correction term
    val A4 = 0.214 * sin(torad(2 * MmP))

    // Corrected longitude
    val lP = moon_longitude + evection + mEc - annual_eq + A4

    // Variation
    val variation = 0.6583 * sin(torad(2 * (lP - lambda_sun)))

    // True longitude
    val lPP = lP + variation

    ///////
    //
    // Calculation of the phase of the Moon

    // Age of the Moon, in degrees
    val moon_age = lPP - lambda_sun

    // Phase of the Moon
    val moon_phase = (1 - cos(torad(moon_age))) / 2.0

    // Calculate distance of Moon from the centre of the Earth
    val moon_dist = (moon_smaxis * (1 - pow(moon_eccentricity, 2))) / (1 + moon_eccentricity * cos(torad(MmP + mEc)))

    // Calculate Moon's angular diameter
    val moon_diam_frac = moon_dist / moon_smaxis
    val moon_angular_diameter = moon_angular_size / moon_diam_frac

    new {
      val phase = fixangle(moon_age) / 360.0
      val illuminated = moon_phase
      val age = synodic_month * fixangle(moon_age) / 360.0
      val distance = moon_dist
      val angular_diameter = moon_angular_diameter
      val sun_distance = sun_dist
      val sun_angular_diameters = sun_angular_diameter
    }
  }
}
class MoonPhase {
  import MoonPhase._

}
