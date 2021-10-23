package org.iyunbo.coding
package parallel

import java.util.concurrent.{ExecutorService, Future, TimeUnit}


object Par {
  type Par[A] = ExecutorService => Future[A]

  private class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(): A = get

    override def get(timeout: Long, unit: TimeUnit): A = get()
  }

  def unit[A](a: A): Par[A] = s => new UnitFuture[A](a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(par: Par[A]): Future[A] = par(s)

  def map2[A, B, C](a: Par[A], b: Par[B])(combine: (A, B) => C): Par[C] = s => {
    val futureA = a(s)
    val futureB = b(s)
    new UnitFuture(combine(futureA.get(), futureB.get()))
  }

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(()))((a, b) => f(a))

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(combine: (A, B, C) => D): Par[D] = {
    val pair: Par[(A, B)] = map2(a, b)((a, b) => (a, b))
    map2(pair, c)((pair, c) => combine(pair._1, pair._2, c))
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(combine: (A, B, C, D) => E): Par[E] = {
    val pairAB: Par[(A, B)] = map2(a, b)((a, b) => (a, b))
    val pairCD: Par[(C, D)] = map2(c, d)((c, d) => (c, d))
    map2(pairAB, pairCD)((pairAB, pairCD) => combine(pairAB._1, pairAB._2, pairCD._1, pairCD._2))
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(combine: (A, B, C, D, E) => F): Par[F] = {
    val pairABCD: Par[(A, B, C, D)] = map4(a, b, c, d)((a, b, c, d) => (a, b, c, d))
    map2(pairABCD, e)((pairABCD, e) => combine(pairABCD._1, pairABCD._2, pairABCD._3, pairABCD._4, e))
  }

  def fork[A](a: => Par[A]): Par[A] = s => s.submit(() => a(s).get())

  def sum(ints: IndexedSeq[Int], minimumFolkCount: Int = 10): Par[Int] = aggregate(ints)(_ + _, 0)

  def max(ints: IndexedSeq[Int], minimumFolkCount: Int = 10): Par[Int] = aggregate(ints)(_ max _, Int.MinValue)

  def wordsCount(paragraphs: List[String]): Par[Int] = aggregate2(paragraphs.toIndexedSeq)(
    _ + _,
    (paragraph, count) => count + paragraph.split("\\s").length, 0
  )

  def aggregate[A](elements: IndexedSeq[A], minimumFolkCount: Int = 10)(combine: (A, A) => A, zero: A): Par[A] =
    aggregate2(elements, minimumFolkCount)(combine, combine, zero)

  def aggregate2[A, B](elements: IndexedSeq[A], minimumFolkCount: Int = 10)(bb: (B, B) => B, ab: (A, B) => B, zero: B): Par[B] = {
    if (elements.size <= minimumFolkCount)
      lazyUnit(elements.foldRight(zero)((a, b) => ab(a, b)))
    else {
      val (l, r) = elements.splitAt(elements.length / 2)
      map2(
        fork(aggregate2(l, minimumFolkCount)(bb, ab, zero)),
        fork(aggregate2(r, minimumFolkCount)(bb, ab, zero))
      )(bb)
    }
  }

  def asyncF[A, B](f: A => B): A => Par[B] = a =>
    map2(unit(a), unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((pa, paList) => map2(pa, paList)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = {
    val parList = parMap(ps)((a: A) => if (f(a)) List(a) else List[A]())
    map(parList)(_.flatten)
  }
}


