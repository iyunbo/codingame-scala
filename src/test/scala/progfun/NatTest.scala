package org.iyunbo.coding
package progfun

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class NatTest extends AnyFlatSpec with should.Matchers {

  it should "create several natural numbers" in {
    val zero = Zero
    val one = new Succ(zero)
    val two = one.successor
    one.predecessor should be(zero)
    two.predecessor should be(one)
    zero.successor should be(one)
    one.successor should be(two)
  }

  it should "calculate natural numbers" in {
    val zero = Zero
    val one = new Succ(zero)
    val two = one.successor
    val three = two.successor
    val four = three.successor
    one + two should be(three)
    one + one should be(two)
    two + zero should be(two)
    two - zero should be(two)
    three - one should be(two)
    three - three should be(zero)
    four - one should be(three)
  }

  it should "print natural numbers" in {
    val zero = Zero
    val one = new Succ(zero)
    val two = one.successor
    val three = two.successor
    val four = three.successor
    zero.toString should be("0")
    one.toString should be("1")
    two.toString should be("2")
    three.toString should be("3")
    four.toString should be("4")
  }


}
