package org.iyunbo.coding
package quickcheck

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{const, oneOf}
import org.scalacheck.Prop._
import org.scalacheck._


import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def extract(h: H): List[Int] = h match {
    case e if isEmpty(e) => List()
    case nonEmpty =>
      val min = findMin(nonEmpty)
      val remaining = deleteMin(nonEmpty)
      min :: extract(remaining)
  }

  property("addMin") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOf2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == Math.min(a, b)
  }

  property("delete1") = forAll { (h: H) =>
    if (!isEmpty(h)) {
      val h2 = deleteMin(h)
      val l1 = extract(h2)
      l1.sorted == l1
    } else {
      true
    }
  }


  property("add1ToEmpty") = forAll { (a: Int) =>
    val h1 = insert(a, empty)
    val h = deleteMin(h1)
    isEmpty(h)
  }

  property("sort1") = forAll { (h: H) =>
    val li = extract(h)
    li == li.sorted
  }

  property("minAndMeld") = forAll { (h1: H, h2: H) =>
    val merged = meld(h1, h2)
    if (isEmpty(h1) && isEmpty(h2)) {
      isEmpty(merged)
    } else if (isEmpty(h1)) {
      findMin(h2) == findMin(merged)
    } else if (isEmpty(h2)) {
      findMin(h1) == findMin(merged)
    } else {
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val min = Math.min(min1, min2)
      min == findMin(merged)
    }
  }

  property("addMore") = forAll { (li: List[Int]) =>
    @tailrec
    def add(li: List[Int], h: H): H = {
      li match {
        case Nil => h
        case l :: ls => add(ls, insert(l, h))
      }
    }

    li.sorted == extract(add(li, empty))
  }

}
