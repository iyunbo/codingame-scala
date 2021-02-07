package org.iyunbo.coding

object MonitorEvaluator {

  case class Ratio(width: Double = 16, height: Double = 9)

  def ppi(ratio: Ratio = Ratio(), widthResolution: Int, size: Double): Double = {
    widthResolution / (size * ratio.height / Math.sqrt(ratio.width * ratio.width + ratio.height * ratio.height))
  }

}
