package org.iyunbo.coding
package hardware

object MonitorEvaluator {

  case class Ratio(width: Double = 16, height: Double = 9)

  def ppi(ratio: Ratio = Ratio(), heightResolution: Int, size: Double): Double = {
    heightResolution / (size * ratio.height / Math.sqrt(ratio.width * ratio.width + ratio.height * ratio.height))
  }

}
