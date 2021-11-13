package org.iyunbo.coding
package pbt

import pbt.Prop.{FailedCase, SuccessCount, TestCases}
import state.RNG

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Passed    => p.run(n, rng)
      case f: Failed => f
    }
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
}

sealed trait Result {
  def isFailed: Boolean
}

case object Passed extends Result {
  override def isFailed: Boolean = false
}

case class Failed(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFailed: Boolean = true
}
