package org.fathens.astronomy

import scala.annotation.tailrec
import org.fathens.math._

object Equations {

  /**
   * Solve the equation of Kepler.
   */
  def kepler(m: Radians, ecc: Double) = {
    val epsilon = 1e-6
    @tailrec
    def solve(e: Radians): Radians = {
      val delta = e - m - Radians(ecc * sin(e))
      val next = e - delta / (1.0 - ecc * cos(e))
      if (delta.abs.value <= epsilon) next
      else solve(next)
    }
    solve(m)
  }
}