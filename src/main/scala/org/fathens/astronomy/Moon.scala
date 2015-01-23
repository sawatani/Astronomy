package org.fathens.astronomy

import java.util.Date

import org.fathens.math._

import com.typesafe.scalalogging.LazyLogging

object Moon extends LazyLogging {

  def at(date: Date) = {
    val sun = Sun(date)
    val day = Days from1980 date

    ////////
    //
    // Calculation of the Moon's position

    // Moon's mean longitude
    val moon_longitude = Radians(0.22997150421858628) * day + moon_mean_longitude_epoch

    // Moon's mean anomaly
    val MM = moon_longitude - Radians(0.001944368345221015) * day - moon_mean_perigee_epoch

    // Moon's ascending node mean longitude
    val evection = Radians(0.022233749341155764) * sin(2 * (moon_longitude - sun.ecliptic_longitude) - MM)

    // Annual equation
    val annual_eq = Radians(0.003242821750205464) * sin(sun.mean_anomaly_perigee)

    val MmP = MM + evection - annual_eq - Radians(0.00645771823237902) * sin(sun.mean_anomaly_perigee)

    // Correction for the equation of the centre
    val mEc = Radians(0.1097567753409154) * sin(MmP)

    // True longitude
    val lPP = {
      // Corrected longitude
      val lP = moon_longitude + evection + mEc - annual_eq + Radians(0.003735004599267865) * sin(2 * MmP)
      // Variation
      val variation = Radians(0.011489502465878671) * sin(2 * (lP - sun.ecliptic_longitude))

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
        MN - Radians(0.0027925268031909274) * sin(sun.mean_anomaly_perigee)
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
      val phase = lPP - sun.ecliptic_longitude
      logger trace f"phase: ${phase}"
      val distance = (moon_smaxis * (1 - (moon_eccentricity ^ 2))) / (1 + moon_eccentricity * cos(MmP + mEc))
    }
  }
}
class Moon(date: Date) {
  import Moon._

  private lazy val coefficients = at(date)

  /**
   * The terminator phase angle as a percentage of a full circle (i.e., 0 to 1)
   */
  lazy val phase = coefficients.phase.normalize / Pi2
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
