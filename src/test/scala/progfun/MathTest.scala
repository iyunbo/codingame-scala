package org.iyunbo.coding
package progfun

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.math.sqrt

class MathTest extends AnyFlatSpec with should.Matchers {
  val TOLERANCE = 0.0001

  it should "calculate the square root of x" in {
    val res = Math.sqrt(36)
    res should be(6.0 +- TOLERANCE)
  }

  it should "calculate the square root of small numbers" in {
    Math.sqrt(0.001) should be(sqrt(0.001) +- TOLERANCE)
    Math.sqrt(0.1e-20) should be(sqrt(0.1e-20) +- TOLERANCE)
  }

  it should "calculate the square root of large numbers" in {
    Math.sqrt(1.0e20) should be(sqrt(1.0e20) +- 0.1)
    Math.sqrt(1.0e50) should be(1.0000000000000725E25 +- 1)
  }

  it should "aggregate in terms of sum" in {
    Math.mapReduce(x => x)(0, (a, b) => a + b)(1, 5) should be(15)
  }

  it should "aggregate in terms of product" in {
    Math.mapReduce(x => x)(1, (a, b) => a * b)(1, 5) should be(120)
  }

}
