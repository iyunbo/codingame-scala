package org.iyunbo.coding
package state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, next) = rng.nextInt
    num match {
      case Int.MinValue => (Int.MaxValue, next)
      case negative: Int if negative < 0 => (-num, next)
      case _ => (num, next)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num, next) = nonNegativeInt(rng)
    (num.toDouble / Int.MaxValue, next)
  }
}

trait State