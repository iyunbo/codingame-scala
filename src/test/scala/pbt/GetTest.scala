package org.iyunbo.coding
package pbt

import state.RNG.SimpleRNG

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GetTest extends AnyFlatSpec with should.Matchers {

  it should "produce an Int" in {
    val result = Gen.int
    val rng = SimpleRNG(42)
    assert(result.sample(rng)._1 > 0)
  }
}
