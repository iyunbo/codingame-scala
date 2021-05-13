package org.iyunbo.coding
package vaccine

object Beep {
  def run(times: Int = 1): Unit = {
    for (i <- 1 to times) {
      java.awt.Toolkit.getDefaultToolkit.beep()
    }
  }

  def main(args: Array[String]): Unit = {
    run()
  }
}
