package org.iyunbo.coding
package reductions

import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var balance = 0;
    for (ch <- chars) {
      if (ch == '(') {
        balance = balance + 1
      } else if (ch == ')') {
        if (balance == 0) return false
        balance = balance - 1
      }
    }
    balance == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, currentLeft: Int, currentRight: Int): Int /*: ???*/ = {
      var left = 0
      var right = 0
      for (i <- idx until until) {
        chars(i) match {
          case '(' => left = left + 1
          case ')' => right = right + 1
          case _ => ()
        }
      }
      left - right
    }

    def reduce(from: Int, until: Int): Int = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + (until - from) / 2
        val (left, right) = parallel(reduce(from, mid), reduce(mid, until))
        left + right
      }
    }

    val res = reduce(0, chars.length)
    if (chars.head == ')') false
    else res == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
