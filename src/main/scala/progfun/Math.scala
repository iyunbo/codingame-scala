package org.iyunbo.coding
package progfun

import scala.annotation.tailrec

object Math {

  def abs(x: Double): Double = if (x < 0) -x else x

  def sqrt(x: Double): Double = {
    def isGoodEnough(guess: Double): Boolean = abs(guess * guess - x) / x < 0.0000000001

    def improve(guess: Double): Double = (guess + x / guess) / 2

    @tailrec
    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    sqrtIter(x / 2)
  }

  def mapReduce(f: Int => Int)(u: Int, combine: (Int, Int) => Int)(a: Int, b: Int): Int = {
    if (a > b) u
    else combine(f(a), mapReduce(f)(u, combine)(a + 1, b))
  }

}
