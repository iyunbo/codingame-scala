package org.iyunbo.coding
package reactive

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class StreamTest extends AnyFlatSpec with should.Matchers {

  it should "convert a stream to list" in {
    Stream(1, 2, 3).toList should be(List(1, 2, 3))
    Stream().toList should be(List())
  }

  it should "take first n elements" in {
    Stream(1, 2, 3, 4, 5, 6).take(3).toList should be(Stream(1, 2, 3).toList)
    Stream().take(3) should be(Empty)
  }

  it should "drop first n elements" in {
    Stream(1, 2, 3, 4, 5, 6).drop(3).toList should be(Stream(4, 5, 6).toList)
    Stream().drop(3) should be(Empty)
  }

  it should "take as many elements as required" in {
    Stream(1, 2, 3, 4, 5, 6).takeWhile(_ % 2 == 0).toList should be(List(2, 4, 6))
    Stream(1, 2, 3, 4, 5, 6).takeWhile(_ % 10 == 0).toList should be(List())
    Stream[Int]().takeWhile(_ => true).toList should be(List())
  }

}
