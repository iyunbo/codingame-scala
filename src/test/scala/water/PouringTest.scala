package org.iyunbo.coding
package water

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PouringTest extends AnyFlatSpec with should.Matchers {

  it should "generate all possible moves" in {
    val waterPouring = new Pouring(Vector(5, 6))
    waterPouring.moves should have size (6)
    val moreComplexWaterPouring = new Pouring(Vector(5, 6, 8))
    moreComplexWaterPouring.moves should have size (12)
  }

  it should "generate path sets of infinite steps" in {
    val waterPouring = new Pouring(Vector(5, 6))
    println(waterPouring.pathSets)
  }

  it should "calculate the solutions" in {
    val pouring = new Pouring(Vector(4, 9, 19, 13, 25))
    println(pouring.solution(11).take(10) mkString "\n")
  }
}
