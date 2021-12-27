package org.iyunbo.coding
package monoid

import pbt.{Gen, Prop}

object Monoid {

  def compose[A](f1: A => A, f2: A => A): A => A = a => f1(f2(a))

  def verifyWithInts(predicate: Int => Boolean): Unit = for (v <- 1 to 999)
    assert(predicate(v))

  type Func = Int => Int

  // These are our elements
  val f: Func = (x: Int) => x + 1
  val g: Func = (x: Int) => x + 2
  val h: Func = (x: Int) => x + 3

  def experiment(): Unit = {

    // This is our neutral element, a function which does nothing
    val id = identity[Int] _

    // This is the composition
    val composition = f(g(h(1))) // 7

    // Which is associative by definition...
    val f1 = compose(f, compose(g, h))
    val f2 = compose(compose(f, g), h)
    verifyWithInts(a => f1(a) == f2(a))

    // If we compose a function with "id", it's like calling the function directly
    val f3 = compose(f, id)
    verifyWithInts(a => f3(a) == f(a))
  }

  def experiment2(): Unit = {

    val composed = f.andThen(g)

    verifyWithInts(a => composed(a) == compose(f, g)(a))
  }

  def experimentMonoid(): Unit = {

    // identity element
    val id = identity[Func] _

    // binary composition operation
    val op: (Func, Func) => Func = (f1: Func, f2: Func) => compose(f1, f2)

    // associativity
    val g_fh = op(g, op(f, h))

    val fh_g = op(op(f, h), g)

    verifyWithInts(a => g_fh(a) == fh_g(a))
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    Prop.forAll(gen) { a =>
      m.op(a, m.zero) == a && m.op(m.zero, a) == a
    }

    Prop.forAll(gen.listOf(Gen.unit(3))) { values =>
      {
        val a = values.toIndexedSeq
        m.op(a(0), m.op(a(1), a(2))) == m.op(m.op(a(0), a(1)), a(2))
      }
    }
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s1), Part(l, n, r)) => Part(s1+l, n, r)
      case (Part(l,n, r), Stub(s2)) => Part(l, n, r + s2)
      case (Part(l1, n1, r1), Part(l2, n2, r2)) => Part(l1, n1 + n2, r1 + l2)
    }

    override def zero: WC = Stub("")

//    def merge(s1: String, s2: String): WC = if(s1.endsWith(" ") || s2.startsWith(" ")) Part(s1, )
  }

}

trait Monoid[A] {
  self =>
  def op(a1: A, a2: A): A
  def zero: A

  def optionMonoid: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
      case (Some(v1), Some(v2)) => Some(self.op(v1, v2))
      case (_, _)               => None
    }

    override def zero: Option[A] = None
  }

  def edoMonoid: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a => self.op(a1(a), a2(a))

    override def zero: A => A = a => a
  }

  def concatenate(as: List[A]): A = as.foldLeft(zero)(op)

  def foldMap[B](as: List[A], m: Monoid[B])(f: A => B): B =
    m.concatenate(as map f)
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

