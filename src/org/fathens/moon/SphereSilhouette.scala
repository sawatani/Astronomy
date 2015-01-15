package org.fathens.moon

import math._

object SphereSilhouette {
  /**
   * true: Light
   * false: Dark
   * None: Out of Circle
   */
  def on(r: Double)(denominator: Int)(numerator: Int)(x: Double, y: Double): Option[Boolean] = {
    val radians = {
      val a = Pi * 2 * numerator / denominator
      if (a < Pi) a else a + Pi
    }
    if (y < -r || r < y) None else {
      val w = sqrt(pow(r, 2) - pow(y, 2))
      val b = w * cos(radians)
      if (x < -w || w < x) None else Some(
        (x < b) ^ (radians < Pi)
      )
    }
  }
}
