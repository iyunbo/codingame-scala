package org.iyunbo.coding
package monoid

import monoid.Monoid.monoidLaws
import pbt.{Gen, Prop}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MonoidTest extends AnyFlatSpec with should.Matchers {

  it should "run basic experiments on Monoid" in {
    Monoid.experiment()
    Monoid.experiment2()
    Monoid.experimentMonoid()
  }

  it should "create basic Monoids" in {
    val intAddition: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2

      override def zero: Int = 0
    }

    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 * a2

      override def zero: Int = 1
    }

    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

      override def zero: Boolean = false
    }

    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

      override def zero: Boolean = true
    }

    List(intAddition, intMultiplication).foreach(monoid =>
      Prop.run(monoidLaws[Int](monoid, Gen.int))
    )

    List(booleanOr, booleanAnd).foreach(monoid =>
      Prop.run(monoidLaws[Boolean](monoid, Gen.boolean))
    )

  }

  it should "count words" in {
    WordCounter.op(WordCounter.zero, Stub("hello world")) should be(
      Part("", 1, "world")
    )
    WordCounter.op(Stub("hello world"), WordCounter.zero) should be(
      Part("hello", 1, "")
    )
    WordCounter.op(Stub("my name is h"), Stub("ello world")) should be(
      Part("my", 3, "world")
    )
    WordCounter.op(Stub("my name is h"), Part("ello", 1, "")) should be(
      Part("my", 4, "")
    )
    WordCounter.op(Stub("my name is h"), Part(" ello", 1, "")) should be(
      Part("my", 5, "")
    )
    WordCounter.op(Part("my", 3, "i"), Stub("s hello world")) should be(
      Part("my", 5, "world")
    )
    WordCounter.op(Part("my", 3, "is "), Stub("hello world")) should be(
      Part("my", 5, "world")
    )
    // TODO: fix Gen and Prop, infinite loop or java.lang.StackOverflowError
//    Prop.run(monoidLaws[WC](WordCounter, Gen.wc))
  }

}
