package org.iyunbo.coding
package progfun

import org.junit.Assert.assertEquals
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RecFunSuite extends AnyFlatSpec with should.Matchers {

  import RecFun._

  // ------ balance tests -----------------------------------------------------

  it should "balance: '(if (zero? x) max (/ 1 x))' is balanced" in {
    balance("(if (zero? x) max (/ 1 x))".toList) should be(true)
  }

  it should "balance: 'I told him ...' is balanced" in {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  it should "balance: ':-)' is unbalanced" in {
    assert(!balance(":-)".toList))
  }

  it should "balance: counting is not enough" in {
    assert(!balance("())(".toList))
  }

  // ------ countChange tests -------------------------------------------------

  it should "countChange: example given in instructions" in {
    assertEquals(3, countChange(4, List(1, 2)))
  }

  it should "countChange: sorted CHF" in {
    assertEquals(1022, countChange(300, List(5, 10, 20, 50, 100, 200, 500)))
  }

  it should "countChange: no pennies" in {
    assertEquals(0, countChange(301, List(5, 10, 20, 50, 100, 200, 500)))
  }

  it should "countChange: unsorted CHF" in {
    assertEquals(1022, countChange(300, List(500, 5, 50, 100, 20, 200, 10)))
  }

  // ------ pascal tests ------------------------------------------------------

  it should "pascal: col=0,row=2" in {
    assertEquals(1, pascal(0, 2))
  }

  it should "pascal: col=1,row=2" in {
    assertEquals(2, pascal(1, 2))
  }

  it should "pascal: col=1,row=3" in {
    assertEquals(3, pascal(1, 3))
  }
}
