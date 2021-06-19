package org.iyunbo.coding
package reactive

import scala.annotation.tailrec

trait PartialFunction[A, R] extends (A => R) {
  def apply(x: A): R

  def isDefinedAt(x: A): Boolean
}

object PartialFunction {
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a: A => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: A => B, g: B => C): A => C = (a: A) => g(f(a))
}

object Lists {
  def lengt[A](as: List[A]): Int = foldLeft(as, 0)((acc, x) => acc + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case List() => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case List() => z
      case _ =>
        val rev = reverse(as)
        foldLeft(rev.tail, f(rev.head, z))((b: B, a: A) => f(a, b))
    }
  }

  def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product(as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, List(): List[A])((acc, x) => x :: acc)

  def append[A](as: List[A], x: A): List[A] = foldRight(as, List(x))((x, acc) => x :: acc)

  def concat[A](as: List[A], bs: List[A]): List[A] = foldRight(as, bs)((x, acc) => x :: acc)

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case List() => List()
    case x :: xs => f(x) :: map(xs)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case List() => List()
    case x :: xs => concat(f(x), flatMap(xs)(f))
  }

  def filter[A](as: List[A])(predicate: A => Boolean): List[A] = flatMap(as)(x => if (predicate(x)) List(x) else List())

  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = (as, bs) match {
    case (xs, List()) => xs
    case (List(), xs) => xs
    case (List(), List()) => List()
    case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
  }


}