package org.iyunbo.coding
package monoid

object Monoid {

  def compose[A](f1: A => A, f2: A => A): A => A = a => f1(f2(a))


  def verifyWithInts(predicate: Int => Boolean): Unit = for (v <- 1 to 999) assert(predicate(v))

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

}
