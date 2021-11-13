package org.iyunbo.coding

import state.RNG

package object pbt {
  def forAll[A](g: Gen[A])(predicate: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(g)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map({ case (a, i) =>
        try {
          if (predicate(a)) Passed else Failed(a.toString, i)
        } catch {
          case e: Exception => Failed(errorMsg(a, e), i)
        }
      })
      .find(_.isFailed)
      .getOrElse(Passed)
  }

  private def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.apply(rng)))

  private def errorMsg[A](a: A, e: Exception): String =
    s"""test case: $a
       |generated an exception: ${e.getMessage}
       |stack trace:
       |${e.getStackTrace.mkString("\n")}""".stripMargin
}
