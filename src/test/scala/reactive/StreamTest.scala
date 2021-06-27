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

  it should "test all elements in Stream" in {
    Stream(1, 3, 5, 7).forAll(_ % 2 == 1) should be(true)
    Stream[Int]().forAll(_ % 2 == 1) should be(true)
    Stream(1, 2, 3, 5, 7).forAll(_ % 2 == 1) should be(false)
  }

  it should "get optional head" in {
    Stream(1, 2).headOption should be(Some(1))
    Stream(1).headOption should be(Some(1))
    Stream().headOption should be(None)
  }

  it should "map stream of type A to stream of type B" in {
    Stream[Int]().map(_.toString) should be(Stream())
    Stream(1, 2, 3).map(_.toString).toList should be(Stream("1", "2", "3").toList)
    Stream("1", "2", "3").map(_.toInt).toList should be(Stream(1, 2, 3).toList)
  }

  it should "filter elements of a Stream" in {
    Stream[Int]().filter(_ % 2 == 0) should be(Stream())
    Stream(1, 2, 3).filter(_ % 2 == 0).toList should be(Stream(2).toList)
    Stream(1, 2, 3).filter(_ % 4 == 0) should be(Stream())
  }

  it should "append an element to a Stream" in {
    Stream[Int]().append(1).toList should be(Stream(1).toList)
    Stream(1, 2).append(3).toList should be(Stream(1, 2, 3).toList)
    Stream().append(3).toList should be(Stream(3).toList)
  }

  it should "concatenate two Streams" in {
    Stream() concat Stream() should be(Stream())
    (Stream(1, 2, 3, 4) concat Stream(5, 6, 7)).toList should be(Stream(1, 2, 3, 4, 5, 6, 7).toList)
    (Stream(1, 2, 3, 4) concat Stream()).toList should be(Stream(1, 2, 3, 4).toList)
    (Stream() concat Stream(4, 5, 6)).toList should be(Stream(4, 5, 6).toList)
  }

  it should "flat map a Stream" in {
    (Stream(1, 2, 3, 4) flatMap (a => Stream[Int](a, a * 2))).toList should be(Stream(1, 2, 2, 4, 3, 6, 4, 8).toList)
    (Stream[Int]() flatMap (a => Stream[Int](a, a * 2))) should be(Stream())
  }

}
