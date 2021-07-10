package org.iyunbo.coding
package state

import state.State.State

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  type Rand[+A] = State[RNG, A]

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => rng => (f(a), rng))

  def nonNegativeInt: Rand[Int] = rng => {
    val (num, next) = rng.nextInt
    num match {
      case Int.MinValue => (Int.MaxValue, next)
      case negative: Int if negative < 0 => (-num, next)
      case _ => (num, next)
    }
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] = map(nonNegativeInt)(_ / Int.MaxValue)

  def intDouble: Rand[(Int, Double)] = map2(int, double)((_, _))

  def doubleInt: Rand[(Double, Int)] = map2(double, int)((_, _))

  def double3: Rand[(Double, Double, Double)] = map3(double, double, double)((_, _, _))

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(
    a => map(rb)(
      b => f(a, b)
    )
  )

  def map3[A, B, C, D](ra: Rand[A], rb: Rand[B], rc: Rand[C])(f: (A, B, C) => D): Rand[D] = flatMap(ra)(
    a => flatMap(rb) {
      b =>
        map(rc) {
          c => f(a, b, c)
        }
    }
  )

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, next) = r(rng)
    f(a)(next)
  }


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = State.sequence[RNG, A](fs)

  def nonNegativeLessThen(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      rng => (mod, rng)
    else
      nonNegativeLessThen(n)
  })

  def rollDie: Rand[Int] = map(nonNegativeLessThen(6))(_ + 1)
}

object State {
  type State[S, +A] = S => (A, S)

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] = st => {
    val (a, next) = s(st)
    (f(a), next)
  }

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(sa)(
    a => map(sb)(b => f(a, b))
  )

  def flatMap[S, A, B](sa: State[S, A])(f: A => State[S, B]): State[S, B] = st => {
    val (a, next) = sa(st)
    f(a)(next)
  }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = st => {
    fs match {
      case List() => (List(), st)
      case x :: xs =>
        val (first, next) = x(st)
        val (list, last) = sequence(xs)(next)
        (first :: list, last)
    }
  }

}