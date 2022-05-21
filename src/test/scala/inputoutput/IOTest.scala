package org.iyunbo.coding
package inputoutput

import org.scalatest.freespec.AnyFreeSpec

class IOTest extends AnyFreeSpec {
  "temperature converter" - {
    val converter: IO[Unit] = for {
      _ <- IO.printLine("Enter a temperature in degrees Fahrenheit: ")
      d <- IO.readLine.map(_.toDouble)
      _ <- IO.printLine(((d - 32) * 5.0 / 9.0).toString)
    } yield ()
  }
}
