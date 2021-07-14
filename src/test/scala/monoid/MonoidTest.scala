package org.iyunbo.coding
package monoid

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MonoidTest extends AnyFlatSpec with should.Matchers {

  it should "run basic experiments on Monoid" in {
    Monoid.experiment()
    Monoid.experiment2()
    Monoid.experimentMonoid()
  }

}
