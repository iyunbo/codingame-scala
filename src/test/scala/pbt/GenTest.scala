package org.iyunbo.coding
package pbt

import parallel.Par
import state.RNG.SimpleRNG

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

class GenTest extends AnyFlatSpec with should.Matchers {

  it should "produce an Int" in {
    val result = Gen.int
    val rng = SimpleRNG(42)
    assert(result.sample(rng)._1 > 0)
  }

  it should "verify sum of list" in {
    val smallInt = Gen.choose(0, 10)
    val spec = Prop.forAll(Gen.listOf(smallInt))(ns => {
      val sum = ns.sum
      !ns.exists(_ > sum)
    })
    Prop.run(spec)
  }

  it should "verify max of list" in {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(Gen.listOf1(smallInt))(ns => {
      val max = ns.max
      !ns.exists(_ > max)
    })
    Prop.run(maxProp)
  }

  it should "verify sorting of list" in {
    val smallInt = Gen.choose(-10, 10)
    @tailrec
    def isSorted(l: List[Int]): Boolean = l match {
      case List()  => true
      case List(_) => true
      case x :: xs => x <= xs.head && isSorted(xs)
    }
    val spec = Prop.forAll(Gen.listOf1(smallInt))(ns => isSorted(ns.sorted))
    Prop.run(spec)
  }

  it should "prove simple property" in {
    Prop.run(Prop.check(true))
  }

  it should "check simple Par properties" in {
    val base1 = Prop.checkPar {
      Par.equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    }
    Prop.run(base1)
  }
}
