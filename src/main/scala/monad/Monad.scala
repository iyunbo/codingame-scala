package org.iyunbo.coding
package monad

import parallel.Par
import parallel.Par.Par

import org.scalacheck.Gen

trait Monad[F[_]] extends Functor[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldLeft(unit(List[A]()))((fl: F[List[A]], fa: F[A]) =>
      flatMap(fl)(l => map(fa)(a => a :: l))
    )

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la map f)

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(
    (1 to n).map(_ => ma).toList
  )

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val r = traverse(ms)(a =>
      map(f(a)) {
        case true  => Some(a)
        case false => None
      }
    )
    map(r)(ll => ll.flatten)
  }
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
}
