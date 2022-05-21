package org.iyunbo.coding
package inputoutput

import inputoutput.IO.Console
import org.scalatest.freespec.AnyFreeSpec

class IOTest extends AnyFreeSpec {
  "temperature converter" - {
    val converter = for {
      _ <- Console.printLn("Enter a temperature in degrees Fahrenheit: ")
      d <- Console.readLn.map(_ map (_.toDouble))
      _ <- d match {
        case Some(dd) => Console.printLn(((dd - 32) * 5.0 / 9.0).toString)
        case _        => Console.printLn("noting to display")
      }
    } yield ()
  }
}
