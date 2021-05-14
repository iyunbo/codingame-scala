package org.iyunbo.coding
package vaccine

object Beep {
  def run(times: Int = 1): Unit = {
    for (i <- 1 to times) {
      Thread.sleep(500)
      java.awt.Toolkit.getDefaultToolkit.beep()
    }
  }

  def long(): Unit = run(2)

  def short(): Unit = run()

  def main(args: Array[String]): Unit = {
    long()
  }
}
