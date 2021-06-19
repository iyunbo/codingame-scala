package org.iyunbo.coding
package reactive

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PartialFunctionTest extends AnyFlatSpec with should.Matchers {
  it should "test partial function" in {
    trait Coin {}
    case class Gold() extends Coin {}
    case class Silver() extends Coin {}

    //    val pf: PartialFunction[Coin, String] = {
    //      case Gold() => "a golden coin"
    //      // no case for Silver(), because we're only interested in Gold()
    //    }
    //
    //    println(pf.isDefinedAt(Gold()))   // true
    //    println(pf.isDefinedAt(Silver())) // false
    //    println(pf(Gold()))               // a golden coin
    //    println(pf(Silver()))
  }

  it should "calculate the length of a List" in {
    import reactive.Lists.lengt

    lengt(List(1, 2, 3, 4, 5)) should be(5)
    lengt(List()) should be(0)
  }

  it should "reverse the elements of a List" in {
    Lists.reverse(List(1, 2, 3)) should be(List(3, 2, 1))
  }

  it should "work with foldRight" in {
    Lists.foldRight(List(1, 2, 3), 0)(_ + _) should be(6)
    Lists.foldRight(List(1, 2, 3), 1)(_ * _) should be(6)
    Lists.foldRight(List("1", "2", "3", "4"), "#!#")(_ + _) should be("1234#!#")
  }

  it should "append an element in a List" in {
    Lists.append(List(1, 2, 3), 4) should be(List(1, 2, 3, 4))
    Lists.append(List(), 4) should be(List(4))
    Lists.append(Nil, 4) should be(List(4))
  }

  it should "concat two Lists" in {
    Lists.concat(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))
    Lists.concat(List(), List(4, 5, 6)) should be(List(4, 5, 6))
    Lists.concat(Nil, List(4, 5, 6)) should be(List(4, 5, 6))
    Lists.concat(Nil, List()) should be(List())
  }

  it should "add 1 to each element of a List" in {
    Lists.map(List(1, 2, 3))((x: Int) => x + 1) should be(List(2, 3, 4))
    Lists.map(List())((x: Int) => x + 1) should be(List())
  }

  it should "add turn each element to string in a List" in {
    Lists.map(List(1.0, 2.0, 3.0))((x: Double) => x.toString) should be(List("1.0", "2.0", "3.0"))
    Lists.map(List())((x: Double) => x.toString) should be(List())
  }

  it should "keep only odd element in the List" in {
    Lists.filter(List(1, 2, 3, 4, 5, 6, 7))(_ % 2 == 1) should be(List(1, 3, 5, 7))
    Lists.filter(List(): List[Int])(_ % 2 == 1) should be(List())
    Lists.filter(List(2, 6, 8))(_ % 2 == 1) should be(List())
  }

  it should "flat map a list of lists" in {
    Lists.flatMap(List(1, 2, 3))(x => List(x, x)) should be(List(1, 1, 2, 2, 3, 3))
    Lists.flatMap(List(): List[Int])(x => List(x, x)) should be(List())
  }

  it should "zip two list with sum of each positoin" in {
    Lists.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) should be(List(5, 7, 9))
    Lists.zipWith(List(), List(4, 5, 6))(_ + _) should be(List(4, 5, 6))
    Lists.zipWith(List(1), List(4, 5, 6))(_ + _) should be(List(5, 5, 6))
    Lists.zipWith(List(1, 2, 3), List(4, 5))(_ + _) should be(List(5, 7, 3))
  }

}
