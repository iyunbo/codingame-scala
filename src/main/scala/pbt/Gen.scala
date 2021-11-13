package org.iyunbo.coding
package pbt

import state.RNG.Rand
import state.{RNG, State}

case class Gen[+A](sample: Rand[A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(
    State.flatMap(sample)(a => f(a).sample)
  )
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)

}

object Gen {
  def choose(start: Int, stop: Int): Gen[Int] = Gen(
    State.map(RNG.nonNegativeInt)(n => start + n % (stop - start))
  )

  def unit[A](a: => A): Gen[A] = Gen(
    State.unit(a)
  )

  def boolean: Gen[Boolean] = Gen(
    State.map(RNG.nonNegativeInt)(n => n % 2 == 0)
  )

  def listOfN[A](size: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(
      State.sequence(List.fill(size)(g.sample))
    )
  }

  def int: Gen[Int] = Gen(RNG.nonNegativeInt)

  def double: Gen[Double] = Gen(RNG.double)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(p => if (p) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2 / g1._2 + g2._2
    Gen(RNG.double).flatMap(d => if (d < threshold) g1._1 else g2._1)
  }

}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = this(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen((n: Int) => this(n).flatMap(f(_)(n)))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))
}
