package org.fathens.astronomy

import java.util.Date

object Days {
  def iso8601 = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
  /**
   * Calculate Astronomic Jujian Days Number
   */
  def jdn(date: Date) = 2440587.5 + date.getTime / (24 * 60 * 60 * 1000.0)
  /**
   * From 1980 January 0.0 in Astronomic Jujian Days Number
   */
  def from1980(date: Date) = jdn(date) - 2444238.5
  /**
   * Base date for E. W. Brown's numbered series of lunations (1923 January 16)
   */
  val lunations_base = 2423436.0
  /**
   * Days of one year
   */
  val one_year = 365.2422
}