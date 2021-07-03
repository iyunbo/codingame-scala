package org.iyunbo.coding
package state

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class StateTest extends AnyFlatSpec with should.Matchers {

  it should "generate non negative integers" in {
    val seed = RNG.SimpleRNG(0)
    val (n1, next1) = RNG.nonNegativeInt(seed)
    n1 should be >= 0
    val (n2, next2) = RNG.nonNegativeInt(next1)
    n2 should be >= 0
    val values = reactive.Stream.unfold[Int, RNG](seed: RNG)(s => reactive.Some(RNG.nonNegativeInt(s))).take(10)
    values.forAll(_ >= 0) should be(true)
  }

  it should "generate double numbers from 0 to 1" in {
    val seed = RNG.SimpleRNG(0)
    val (n1, next1) = RNG.double(seed)
    n1 should be >= 0.0
    n1 should be < 1.0
    val (n2, next2) = RNG.double(next1)
    n2 should be >= 0.0
    n2 should be < 1.0
    val values = reactive.Stream.unfold[Double, RNG](seed: RNG)(s => reactive.Some(RNG.double(s))).take(10)
    values.forAll(d => d >= 0.0 && d < 1.0) should be(true)
  }

  it should "generate int and double tuples" in {
    val seed = RNG.SimpleRNG(0)
    val ((i1, d1), next1) = RNG.intDouble(seed)
    i1 shouldBe a[Int]
    d1 shouldBe a[Double]
    val ((d2, i2), next2) = RNG.doubleInt(next1)
    i2 shouldBe a[Int]
    d2 shouldBe a[Double]
    val values = reactive.Stream.unfold[(Double, Double, Double), RNG](seed: RNG)(s => reactive.Some(RNG.double3(s))).take(10)
    values.forAll(d => d._1 >= 0.0 && d._2 < 1.0 && d._3 < 1.0) should be(true)
  }

  it should "generate list of ints" in {
    val seed = RNG.SimpleRNG(0)
    val (l, next1) = RNG.ints(3)(seed)
    l should have size 3
    l.head should be(0)
    println(l)
  }


}
