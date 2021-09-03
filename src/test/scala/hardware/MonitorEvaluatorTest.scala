package org.iyunbo.coding
package hardware

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MonitorEvaluatorTest extends AnyFlatSpec with should.Matchers {
  it should "evaluate ppi of a monitor" in {
    val ppi = MonitorEvaluator.ppi(heightResolution = 1440, size = 32.0)
    ppi should be(91.79 +- 0.01)
  }

  it should "evaluate ppi of my Samsung monitor" in {
    val ppi = MonitorEvaluator.ppi(heightResolution = 2160, size = 32.0)
    println(s"your ppi is: $ppi")
  }

  it should "evaluate ppi of a 27' LG 27MD5KL monitor" in {
    val ppi = MonitorEvaluator.ppi(heightResolution = 2880, size = 27.0)
    println(s"your ppi is: $ppi")
  }

  it should "evaluate ppi of a 24' LG 24MD4KL monitor" in {
    val ppi = MonitorEvaluator.ppi(heightResolution = 2160, size = 23.7)
    println(s"your ppi is: $ppi")
  }

  it should "evaluate ppi of a 34' Mi Curved Gaming monitor" in {
    val ppi = MonitorEvaluator.ppi(heightResolution = 1440, size = 34)
    println(s"your ppi is: $ppi")
  }

  it should "evaluate ppi of a 150' Mi 4k Lazer Project" in {
    val ppi = MonitorEvaluator.ppi(heightResolution = 2160, size = 80)
    println(s"your ppi is: $ppi")
  }

}
