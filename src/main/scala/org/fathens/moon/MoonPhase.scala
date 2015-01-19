package org.fathens.moon

import java.util.Date

import scala.math._

object MoonDemo extends App with Logging {
  args.map(Astronomic.Days.iso8601.parse).foreach { date =>
    Log warn f"Start calculating: ======== ${Astronomic.Days.iso8601 format date} ========"
    Log info f"${new MoonPhase(date)}"
  }
}
object MoonPhase extends Logging {
  import Astronomic._

  def at(date: Date) = {
    val day = Days from1980 date

    // Convert from perigee coordinates to epoch 1980
    val M = {
      // Mean anomaly of the Sun
      val N = fixangle((360 / 365.2422) * day)
      Log trace f"N: ${N}"
      fixangle(N + ecliptic_longitude_epoch - ecliptic_longitude_perigee)
    }
    Log trace f"day: ${day}, M: ${M}"

    val sun = new {
      // True anomaly
      private val Ec = {
        // Solve Kepler's equation
        val a = kepler(M, eccentricity)
        Log debug f"Kepler's equation(M: ${M}%3.5f, eccentricity: ${eccentricity})=${a}%3.5f"
        val b = sqrt((1 + eccentricity) / (1 - eccentricity)) * tan(a / 2.0)
        2 * atan(b).toDegrees
      }
      Log trace f"sun's Ec=${Ec}"
      // Orbital distance factor
      private val F = ((1 + eccentricity * cos(Ec.toRadians)) / (1 - pow(eccentricity, 2)))

      // Distance to Sun in km
      val distance = sun_smaxis / F
      val angular_diameter = F * sun_angular_size_smaxis

      // Suns's geometric ecliptic longuitude
      val eclipticLongitude = fixangle(Ec + ecliptic_longitude_perigee)
    }
    Log trace f"sun(eclipticLongitude: ${sun.eclipticLongitude})"

    ////////
    //
    // Calculation of the Moon's position

    // Moon's mean longitude
    val moon_longitude = fixangle(13.1763966 * day + moon_mean_longitude_epoch)

    // Moon's mean anomaly
    val MM = fixangle(moon_longitude - 0.1114041 * day - moon_mean_perigee_epoch)

    // Moon's ascending node mean longitude
    val evection = 1.2739 * sin((2 * (moon_longitude - sun.eclipticLongitude) - MM).toRadians)

    // Annual equation
    val annual_eq = 0.1858 * sin(M.toRadians)

    val MmP = MM + evection - annual_eq - 0.37 * sin(M.toRadians)

    // Correction for the equation of the centre
    val mEc = 6.2886 * sin(MmP.toRadians)

    // True longitude
    val lPP = {
      // Corrected longitude
      val lP = moon_longitude + evection + mEc - annual_eq + 0.214 * sin((2 * MmP).toRadians)
      // Variation
      val variation = 0.6583 * sin((2 * (lP - sun.eclipticLongitude)).toRadians)

      lP + variation
    }
    Log trace f"lPP=${lPP}"

    ///////
    //
    // Calculation of the Moon's inclination
    // unused for phase calculation.
    val inclination = new {
      // Corrected longitude of the node
      private val NP = {
        // Moon's ascending node mean longitude
        val MN = fixangle(node_mean_longitude_epoch - 0.0529539 * day)
        MN - 0.16 * sin(M.toRadians)
      }

      // Ecliptic longitude
      val lambda_moon = {
        // Y inclination coordinate
        val y = sin((lPP - NP).toRadians) * cos(moon_inclination.toRadians)
        // X inclination coordinate
        val x = cos((lPP - NP).toRadians)

        atan2(y, x).toDegrees + NP
      }

      // Ecliptic latitude
      val BetaM = (asin(sin((lPP - NP).toRadians) * sin(moon_inclination.toRadians))).toDegrees
    }

    new {
      val phase = lPP - sun.eclipticLongitude
      Log trace f"phase: ${phase}"
      val distance = (moon_smaxis * (1 - pow(moon_eccentricity, 2))) / (1 + moon_eccentricity * cos((MmP + mEc).toRadians))
    }
  }
}
class MoonPhase(date: Date) {
  import MoonPhase._
  import Astronomic._

  private lazy val coefficients = at(date)

  /**
   * The terminator phase angle as a percentage of a full circle (i.e., 0 to 1)
   */
  lazy val phase = fixangle(coefficients.phase) / 360.0
  /**
   * The illuminated fraction of the Moon's disc
   */
  lazy val illuminated = (1 - cos(coefficients.phase.toRadians)) / 2.0
  /**
   * Age of the Moon, in degrees
   */
  lazy val age = synodic_month * phase
  /**
   * distance of Moon from the centre of the Earth
   */
  lazy val distance = coefficients.distance
  /**
   * Moon's angular diameter
   */
  lazy val angularDiameter = moon_angular_size * moon_smaxis / distance

  override def toString = {
    f"MoonPhase(illuminated: ${illuminated * 100}%3.5f%%, age: ${age}%02.1f)"
  }
}
