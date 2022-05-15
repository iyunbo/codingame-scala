package org.iyunbo.coding
package monad

import parallel.Par
import parallel.Par.Par
import state.State
import state.State.State

import org.scalacheck.Gen

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldLeft(unit(List[A]()))((fl: F[List[A]], fa: F[A]) =>
      flatMap(fl)(l => map(fa)(a => a :: l))
    )

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    flatMap(fab)(a2b => map(fa)(a2b))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la map f)

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(
    (1 to n).map(_ => ma).toList
  )

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldLeft(unit(List[A]()))((acc, a) =>
      flatMap(f(a))(b => if (b) map2(unit(a), acc)(_ :: _) else acc)
    )
  }

  def compose[A, B, C](fa: A => F[B], fb: B => F[C]): A => F[C] = a =>
    flatMap(fa(a))(fb)

  def flatMapV2[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose(identity[F[A]], f)(fa)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity[F[A]])
}

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa flatMap f

    override def unit[A](a: => A): Gen[A] = Gen.const[A](a)
  }

  val parserMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa flatMap f

    override def unit[A](a: => A): Option[A] = Option.apply(a)
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(fa)(f)

    override def unit[A](a: => A): Par[A] = Par.unit(a)
  }

  def stateMonad[S]: Monad[({ type f[X] = State[S, X] })#f] =
    new Monad[({ type f[X] = State[S, X] })#f] {
      override def flatMap[A, B](fa: State[S, A])(
          f: A => State[S, B]
      ): State[S, B] = State.flatMap(fa)(f)

      override def unit[A](a: => A): State[S, A] = State.unit(a)
    }
}
