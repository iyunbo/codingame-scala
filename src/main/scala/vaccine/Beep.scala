package org.iyunbo.coding
package vaccine

object Beep {
  def run(): Unit = {
    java.awt.Toolkit.getDefaultToolkit.beep()
  }

  def main(args: Array[String]): Unit = {
    run()
  }
}
