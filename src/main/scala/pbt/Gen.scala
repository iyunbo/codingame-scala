package org.iyunbo.coding
package pbt

import parallel.Par.Par
import pbt.Gen.{forAllPar, int}
import pbt.Prop._
import state.RNG.Rand
import state.{RNG, State}

import java.util.concurrent.{ExecutorService, Executors}

case class Gen[+A](sample: Rand[A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(
    State.flatMap(sample)(a => f(a).sample)
  )

  def map[B](f: A => B): Gen[B] = flatMap(a => Gen(State.unit(f(a))))

  def map2[B, C](that: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(State.map2(this.sample, that.sample)(f))

  def listOf(size: Gen[Int]): Gen[List[A]] = size.flatMap(Gen.listOfN(_, this))

  def listOf: Gen[List[A]] = listOf(int)

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_, _))

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

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n + 1, g))

  def int: Gen[Int] = Gen(RNG.nonNegativeInt)

  def double: Gen[Double] = Gen(RNG.double)

  def char: Gen[Char] = for {
    i <- int
    b <- boolean
  } yield {
    if (b) {
      'A' + i % 26
    }.toChar
    else {
      'a' + i % 26
    }.toChar
  }

  def string: Gen[String] = char.listOf.map(_.toString())

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(p => if (p) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2 / g1._2 + g2._2
    Gen(RNG.double).flatMap(d => if (d < threshold) g1._1 else g2._1)
  }

  val S: Gen[ExecutorService] = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> 0.25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen((n: Int) => this(n).flatMap(f(_)(n)))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case f               => f
    }
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](g: Int => Gen[A])(predicate: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max

      val props: LazyList[Prop] =
        LazyList
          .from(0)
          .take((n min max) + 1)
          .map(i => forAll(g(i))(predicate))

      val prop: Prop = props
        .map(p =>
          Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }
        )
        .toList
        .reduce(_ && _)

      prop.run(max, n, rng)
  }

  def forAll[A](g: SGen[A])(predicate: A => Boolean): Prop =
    forAll(g(_))(predicate)

  def run(
      p: Prop,
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = RNG.SimpleRNG(42)
  ): Unit = p.run(maxSize, testCases, rng) match {
    case Failed(msg, n) =>
      println(s"""! Failed after $n passed tests
                 |$msg
                 |""".stripMargin)
    case Passed =>
      println(s"+ OK, passed $testCases tests.")
    case Proved =>
      println(s"+ OK, proved property.")
  }

  def forAll[A](g: Gen[A])(predicate: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(g)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map({ case (a, i) =>
          try {
            if (predicate(a))
              Passed
            else
              Failed(a.toString, i)
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

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Failed("()", 0)
  }

  def checkPar(p: => Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)
}

sealed trait Result {
  def isFailed: Boolean
}

case object Passed extends Result {
  override def isFailed: Boolean = false
}

case object Proved extends Result {
  override def isFailed: Boolean = false
}

case class Failed(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFailed: Boolean = true
}

object ** {
  def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)
}
