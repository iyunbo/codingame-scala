package org.iyunbo.coding
package monad

trait Applicative[F[_]] extends Functor[F] {

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)
}

object Applicative {
  val streamApplicative: Applicative[LazyList] = new Applicative[LazyList] {
    override def apply[A, B](fab: LazyList[A => B])(
        fa: LazyList[A]
    ): LazyList[B] = map2(fab, fa)((a2b, a) => a2b(a))

    override def unit[A](a: => A): LazyList[A] = LazyList.continually(a)
  }
}
