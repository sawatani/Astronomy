package org.fathens.astronomy

import java.util.Date

import org.fathens.math._

import com.typesafe.scalalogging.LazyLogging

object MoonDemo extends App with LazyLogging {
  args.foreach { date =>
    logger warn f"Start calculating: ======== ${date} ========"
    logger info f"${new MoonPhase(Days.iso8601 parse date)}"
  }
}
object MoonPhase extends LazyLogging {

  def circle(a: Radians) = (Pi2 + a % Pi2) % Pi2

  def at(date: Date) = {
    val day = Days from1980 date

    // Convert from perigee coordinates to epoch 1980
    val M = {
      // Mean anomaly of the Sun
      val N = day * Pi2 / Days.one_year
      logger trace f"N: ${N}"
      N + ecliptic_longitude_epoch - ecliptic_longitude_perigee
    }
    logger trace f"Mean anomaly(day: ${day}): ${M}"

    val sun = new {
      // True anomaly
      private val ta = {
        // Eccentric anomaly
        val a = Equations.kepler(M, eccentricity)
        logger debug f"Kepler's equation(Mean anomaly: ${M}, eccentricity: ${eccentricity})=${a}"
        val b = ((1 + eccentricity) / (1 - eccentricity)).sqrt * tan(a / 2.0)
        2 * atan(b)
      }
      logger trace f"Sun's True anomaly: ${ta}"
      // Orbital distance factor
      private val F = ((1 + eccentricity * cos(ta)) / (1 - (eccentricity ^ 2)))

      // Distance to Sun in km
      val distance = sun_smaxis / F
      val angular_diameter = F * sun_angular_size_smaxis

      // Suns's geocentric ecliptic longitude
      val eclipticLongitude = ta + ecliptic_longitude_perigee
    }
    logger trace f"sun(ecliptic_longitude: ${sun.eclipticLongitude})"

    ////////
    //
    // Calculation of the Moon's position

    // Moon's mean longitude
    val moon_longitude = Radians(0.22997150421858628) * day + moon_mean_longitude_epoch

    // Moon's mean anomaly
    val MM = moon_longitude - Radians(0.001944368345221015) * day - moon_mean_perigee_epoch

    // Moon's ascending node mean longitude
    val evection = Radians(0.022233749341155764) * sin(2 * (moon_longitude - sun.eclipticLongitude) - MM)

    // Annual equation
    val annual_eq = Radians(0.003242821750205464) * sin(M)

    val MmP = MM + evection - annual_eq - Radians(0.00645771823237902) * sin(M)

    // Correction for the equation of the centre
    val mEc = Radians(0.1097567753409154) * sin(MmP)

    // True longitude
    val lPP = {
      // Corrected longitude
      val lP = moon_longitude + evection + mEc - annual_eq + Radians(0.003735004599267865) * sin(2 * MmP)
      // Variation
      val variation = Radians(0.011489502465878671) * sin(2 * (lP - sun.eclipticLongitude))

      lP + variation
    }
    logger trace f"lPP=${lPP}"

    ///////
    //
    // Calculation of the Moon's inclination
    // unused for phase calculation.
    val inclination = new {
      // Corrected longitude of the node
      private val NP = {
        // Moon's ascending node mean longitude
        val MN = node_mean_longitude_epoch - Radians(9.242199067718253E-4) * day
        MN - Radians(0.0027925268031909274) * sin(M)
      }

      // Ecliptic longitude
      val lambda_moon = {
        // Y inclination coordinate
        val y = sin(lPP - NP) * cos(moon_inclination)
        // X inclination coordinate
        val x = cos(lPP - NP)

        atan2(y, x) + NP
      }

      // Ecliptic latitude
      val BetaM = asin(sin(lPP - NP) * sin(moon_inclination))
    }
    logger debug f"inclination: lambda_moon: ${inclination.lambda_moon}, BetaM: ${inclination.BetaM}"

    new {
      val phase = lPP - sun.eclipticLongitude
      logger trace f"phase: ${phase}"
      val distance = (moon_smaxis * (1 - (moon_eccentricity ^ 2))) / (1 + moon_eccentricity * cos(MmP + mEc))
    }
  }
}
class MoonPhase(date: Date) {
  import MoonPhase._

  private lazy val coefficients = at(date)

  /**
   * The terminator phase angle as a percentage of a full circle (i.e., 0 to 1)
   */
  lazy val phase = circle(coefficients.phase) / Pi2
  /**
   * The illuminated fraction of the Moon's disc
   */
  lazy val illuminated = (1 - cos(coefficients.phase)) / 2.0
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
  lazy val angularDiameter = moon_smaxis / distance * moon_angular_size

  override def toString = {
    f"MoonPhase(illuminated: ${illuminated * 100}%3.5f%%, age: ${age}%02.5f)"
  }
}
