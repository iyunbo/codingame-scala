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

  def unit[A](a: => A): Par[A] = s => new UnitFuture[A](a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(par: Par[A]): Future[A] = par(s)

  def map2[A, B, C](a: Par[A], b: Par[B])(combine: (A, B) => C): Par[C] = s => {
    val futureA = a(s)
    val futureB = b(s)
    new UnitFuture(combine(futureA.get(), futureB.get()))
  }

  def fork[A](a: => Par[A]): Par[A] = s => s.submit(() => a(s).get())

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
}


