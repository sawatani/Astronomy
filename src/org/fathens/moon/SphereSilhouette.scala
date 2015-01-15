package org.fathens.moon

import math._

object SphereSilhouette {
  def toRadians(denominator: Int)(numerator: Int): Double = {
    Pi * 2 * numerator / denominator
  }
  /**
   * true: Light
   * false: Dark
   * None: Out of Circle
   */
  def on(r: Double)(radians: Double)(x: Double, y: Double): Option[Boolean] = {
    if (y < -r || r < y) None else {
      val w = sqrt(pow(r, 2) - pow(y, 2))
      val b = w * cos(radians)
      if (x < -w || w < x) None else Some(
        (x < b) ^ (radians < Pi)
      )
    }
  }
}
