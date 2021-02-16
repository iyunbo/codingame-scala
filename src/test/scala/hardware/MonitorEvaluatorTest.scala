package org.iyunbo.coding
package hardware

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MonitorEvaluatorTest extends AnyFlatSpec with should.Matchers {
  it should "evaluate ppi of a monitor" in {
    val ppi = MonitorEvaluator.ppi(heightResolution = 1440, size = 32.0)
    ppi should be(91.79 +- 0.01)
  }

  it should "evaluate ppi of a new monitor" in {
    val ppi = MonitorEvaluator.ppi(heightResolution = 2160, size = 32.0)
    println(s"your ppi is: $ppi")
  }
}
