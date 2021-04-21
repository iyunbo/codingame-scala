package org.iyunbo.coding
package frp

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SignalTest extends AnyFlatSpec with should.Matchers {

  it should "create and chain signals" in {
    val sig1 = Signal(600)
    val sig2 = Signal(2 * sig1())
    sig2() should be(1200)

    val sig3 = Var(600)
    val sigDouble = Signal(2 * sig3())
    sigDouble() should be(1200)
    sig3.update(700)
    sigDouble() should be(1400)

    sig3.update(1 + sig1())
    sigDouble() should be(1202)

    val sigPlus5 = Signal(5 + sigDouble())
    sigPlus5() should be(1207)

    sig3.update(200)
    sigPlus5() should be(405)

    val sigComposite = Signal(5 + sigPlus5() + sig2())
    sigComposite() should be(1610)
  }
}
