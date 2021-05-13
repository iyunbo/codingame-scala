package org.iyunbo.coding
package vaccine

object Beep {
  def run(times: Int = 1): Unit = {
    for (i <- 1 to times) {
      Thread.sleep(50)
      java.awt.Toolkit.getDefaultToolkit.beep()
    }
  }

  def long(): Unit = run(30)

  def short(): Unit = run(5)

  def main(args: Array[String]): Unit = {
    run()
  }
}
