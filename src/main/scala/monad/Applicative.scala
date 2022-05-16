package org.iyunbo.coding
package monad

trait Applicative[F[_]] extends Functor[F] {

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))

  def sequence[A](as: List[F[A]]): F[List[A]] = traverse(as)(fa => fa)

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map[K, V]()))((kv, acc) =>
      map2(kv._2, acc)((v, m) => m + (kv._1, v))
    )

  def product[G[_]](
      G: Applicative[G]
  ): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    val self = this
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def apply[A, B](fs: (F[A => B], G[A => B]))(
          p: (F[A], G[A])
      ): (F[B], G[B]) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }
}

object Applicative {
  val streamApplicative: Applicative[LazyList] = new Applicative[LazyList] {
    override def apply[A, B](fab: LazyList[A => B])(
        fa: LazyList[A]
    ): LazyList[B] = fab.flatMap(f => fa.map(f(_)))

    override def unit[A](a: => A): LazyList[A] = LazyList.continually(a)
  }

  def validationApplicative[E]
      : Applicative[({ type f[x] = Validation[E, x] })#f] = new Applicative[
    ({
      type f[x] = Validation[E, x]
    })#f
  ] {
    override def apply[A, B](fab: Validation[E, A => B])(
        fa: Validation[E, A]
    ): Validation[E, B] = (fab, fa) match {
      case (Success(a2b), Success(a))    => Success(a2b(a))
      case (Success(a2b), Failure(h, t)) => Failure(h, t)
      case (Failure(h, t), Success(a))   => Failure(h, t)
      case (Failure(hab, tab), Failure(ha, ta)) =>
        Failure(ha, ta ++ (hab +: tab))
    }

    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]
