package org.iyunbo.coding
package list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ListsTest extends AnyFlatSpec with should.Matchers {

  it should "flat a list of different structures" in {
    Lists.flatten(List(1, 1, 2)) should be(List(1, 1, 2))
    Lists.flatten(List(List(1, 1))) should be(List(1, 1))
    Lists.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be(List(1, 1, 2, 3, 5, 8))
  }
}
