package org.iyunbo.coding
package reactive

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class OptionTest extends AnyFlatSpec with should.Matchers {

  it should "test Option" in {
    Some(9).map(_.toString) should be(Some("9"))
    None.map(_.toString) should be(None)
    Some(1).filter(_ > 1) should be(None)
    Some(1).filter(_ >= 1) should be(Some(1))
    Some(2).getOrElse(0) should be(2)
    None.orElse(Some(1)) should be(Some(1))
  }

  it should "calculate variance" in {
    def variance(xs: Seq[Double]): Option[Double] = {
      def mean(xs: Seq[Double]): Option[Double] = {
        xs match {
          case Seq() => None
          case _ => Some(xs.sum / xs.size)
        }
      }

      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    variance(Seq(1.0, 2.0, 3.0)) should be(Some(2.0 / 3))
  }

  it should "detect None of a list" in {
    Option.sequence(List(Some(1), Some(2))) should be(Some(List(1, 2)))
    Option.sequence(List(Some(1), Some(2), None)) should be(None)
    Option.sequence(List()) should be(Some(List()))
  }
}
