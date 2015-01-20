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
  
  val Pi2 = 2 * Pi
  def circle(a: Double) = (Pi2 + a % Pi2) % Pi2 

  def at(date: Date) = {
    val day = Days from1980 date

    // Convert from perigee coordinates to epoch 1980
    val M = {
      // Mean anomaly of the Sun
      val N = circle(day * Pi2 / days_in_year)
      Log trace f"N: ${N}"
      circle(N + ecliptic_longitude_epoch - ecliptic_longitude_perigee)
    }
    Log trace f"day: ${day}, M: ${M}"

    val sun = new {
      // True anomaly
      private val Ec = {
        // Solve Kepler's equation
        val a = kepler(M, eccentricity)
        Log debug f"Kepler's equation(M: ${M}%3.5f, eccentricity: ${eccentricity})=${a}%3.5f"
        val b = sqrt((1 + eccentricity) / (1 - eccentricity)) * tan(a / 2.0)
        2 * atan(b)
      }
      Log trace f"sun's Ec=${Ec}"
      // Orbital distance factor
      private val F = ((1 + eccentricity * cos(Ec)) / (1 - pow(eccentricity, 2)))

      // Distance to Sun in km
      val distance = sun_smaxis / F
      val angular_diameter = F * sun_angular_size_smaxis

      // Suns's geometric ecliptic longuitude
      val eclipticLongitude = circle(Ec + ecliptic_longitude_perigee)
    }
    Log trace f"sun(eclipticLongitude: ${sun.eclipticLongitude})"

    ////////
    //
    // Calculation of the Moon's position

    // Moon's mean longitude
    val moon_longitude = circle(0.22997150421858628 * day + moon_mean_longitude_epoch)

    // Moon's mean anomaly
    val MM = circle(moon_longitude - 0.001944368345221015 * day - moon_mean_perigee_epoch)

    // Moon's ascending node mean longitude
    val evection = 0.022233749341155764 * sin(2 * (moon_longitude - sun.eclipticLongitude) - MM)

    // Annual equation
    val annual_eq = 0.003242821750205464 * sin(M)

    val MmP = MM + evection - annual_eq - 0.00645771823237902 * sin(M)

    // Correction for the equation of the centre
    val mEc = 0.1097567753409154 * sin(MmP)

    // True longitude
    val lPP = {
      // Corrected longitude
      val lP = moon_longitude + evection + mEc - annual_eq + 0.003735004599267865 * sin(2 * MmP)
      // Variation
      val variation = 0.011489502465878671 * sin(2 * (lP - sun.eclipticLongitude))

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
        val MN = circle(node_mean_longitude_epoch - 9.242199067718253E-4 * day)
        MN - 0.0027925268031909274 * sin(M)
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
    Log debug f"inclination: lambda_moon: ${inclination.lambda_moon}, BetaM: ${inclination.BetaM}"

    new {
      val phase = circle(lPP - sun.eclipticLongitude)
      Log trace f"phase: ${phase}%5.5f"
      val distance = (moon_smaxis * (1 - pow(moon_eccentricity, 2))) / (1 + moon_eccentricity * cos(MmP + mEc))
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
  lazy val phase = coefficients.phase / Pi2
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
  lazy val angularDiameter = moon_angular_size * moon_smaxis / distance

  override def toString = {
    f"MoonPhase(illuminated: ${illuminated * 100}%3.5f%%, age: ${age}%02.5f)"
  }
}
