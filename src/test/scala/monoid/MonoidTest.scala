package org.iyunbo.coding
package monoid

import org.iyunbo.coding.monoid.Monoid.monoidLaws
import org.iyunbo.coding.pbt.{Gen, Prop}
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
      Prop.run(monoidLaws(monoid, Gen.int))
    )

    List(booleanOr, booleanAnd).foreach(monoid =>
      Prop.run(monoidLaws(monoid, Gen.boolean))
    )

  }

}
