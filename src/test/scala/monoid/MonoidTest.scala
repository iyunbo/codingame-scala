package org.iyunbo.coding
package monoid

import monoid.Monoid.{monoidLaw1, monoidLaw2}

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
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

    object IntMonoid extends Properties("Int Monoid") {
      val in: Gen[Int] = Gen.chooseNum(Int.MinValue, Int.MaxValue)
      val ins: Gen[List[Int]] = Gen.listOfN(20, in).suchThat(_.size > 2)

      property("unit") = forAll(in)(i =>
        monoidLaw1(intAddition, i) && monoidLaw1(intMultiplication, i)
      )

      property("associate") = forAll(ins)(is =>
        monoidLaw2(intAddition, is) && monoidLaw2(intMultiplication, is)
      )
    }

    object BooleanMonoid extends Properties("Boolean Monoid") {
      val in: Gen[Boolean] = Gen.size map (_ % 2 == 0)
      val ins: Gen[List[Boolean]] = Gen.listOfN(20, in).suchThat(_.size > 2)

      property("unit") =
        forAll(in)(b => monoidLaw1(booleanAnd, b) && monoidLaw1(booleanOr, b))

      property("associate") = forAll(ins)(bs =>
        monoidLaw2(booleanAnd, bs) && monoidLaw2(booleanOr, bs)
      )
    }

    IntMonoid.check()
    BooleanMonoid.check()

  }

  object WordCounterMonoid extends Properties("WordCounter") {
    val wcGen: Gen[WC] = Gen.oneOf(
      for {
        s <- Gen.alphaStr
      } yield Stub(s),
      for {
        l <- Gen.alphaStr
        n <- Gen.size
        r <- Gen.alphaStr
      } yield Part(l, n, r)
    )

    val wcGens: Gen[List[WC]] = Gen.listOfN(10, wcGen).suchThat(_.size > 2)

    property("unit") = forAll(wcGen)(wc => monoidLaw1(WordCounter, wc))
    property("associative") =
      forAll(wcGens)(wcs => monoidLaw2(WordCounter, wcs))
  }

  it should "count words" in {
    WordCounter.op(WordCounter.zero, Stub("hello world")) should be(
      Stub("hello world")
    )
    WordCounter.op(Stub("hello world"), WordCounter.zero) should be(
      Stub("hello world")
    )
    WordCounter.op(Stub("my name is h"), Stub("ello world")) should be(
      Stub("my name is hello world")
    )
    WordCounter.op(Stub("my name is h"), Part("ello", 1, "")) should be(
      Part("my name is hello", 1, "")
    )
    WordCounter.op(Stub("my name is h"), Part(" ello", 1, "")) should be(
      Part("my name is h ello", 1, "")
    )
    WordCounter.op(Part("my", 3, "i"), Stub("s hello world")) should be(
      Part("my", 3, "is hello world")
    )
    WordCounter.op(Part("my", 3, "is "), Stub("hello world")) should be(
      Part("my", 3, "is hello world")
    )

    WordCounter.op(Part("my", 3, "is "), Part("hello world", 3, "")) should be(
      Part("my", 7, "")
    )
  }

  "WordCounter" should "be a monoid" in {
    WordCounterMonoid.check()
  }
}
