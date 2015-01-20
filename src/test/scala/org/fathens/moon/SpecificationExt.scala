package org.fathens.moon

import org.scalacheck._
import org.specs2._
import org.specs2.matcher._

trait SpecificationExt  extends Specification with DataTables with ScalaCheck {
  /**
   * Extend Double Matchers
   */
  implicit class MoreDouble(a: Double) {
    def must_=~(b: Double)(implicit p: Precision) = a must beCloseTo(b, p.v)
  }
  case class Precision(v: Double)
  /**
   * Date Generator
   */
  val genDate = Gen.choose(0, new java.util.Date().getTime * 2).map(new java.util.Date(_))
}
