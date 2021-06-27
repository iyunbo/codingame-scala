package org.iyunbo.coding
package reactive

import reactive.Stream.{cons, unfold}

sealed trait Stream[+A] {
  def toList: List[A] = folderRight(List[A]())((a, li) => a :: li)

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n <= 0) Empty else Stream.cons(h(), t().take(n - 1))
  }

  def takeUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), n) => if (n > 0) Some(h(), (t(), n - 1)) else None
    case _ => None
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n <= 0) this else t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = folderRight(Stream[A]())((a, s) => if (p(a)) cons(a, s) else s)

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, other)) {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
  }

  //FIXME: incorrect impl
  def takeWhileUnfold(f: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if f(h()) => Some((h(), t()))
    case _ => None
  }

  def folderRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().folderRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = folderRight(true)((a, s) => p(a) && s)

  def exists(p: A => Boolean): Boolean = folderRight(false)((h, t) => p(h) || t)

  def headOption: Option[A] = folderRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = folderRight(Stream[B]())((a, bs) => cons(f(a), bs))

  def mapUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def filter(p: A => Boolean): Stream[A] = folderRight(Stream[A]())((a, bs) => if (p(a)) cons(a, bs) else bs)

  def append[B >: A](a: => B): Stream[B] = folderRight(Stream(a))((x, bs) => cons[B](x, bs))

  def flatMap[B](f: A => Stream[B]): Stream[B] = folderRight(Stream[B]())((a, bs) => f(a) concat bs)

  def concat[B >: A](other: Stream[B]): Stream[B] = folderRight(other)((a, ss) => cons(a, ss))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def tail: Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => t()
  }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, other)) {
    case (Empty, Empty) => None
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def ones: Stream[Int] = cons(1, ones)

  def onesUnfold: Stream[Int] = unfold(1)(s => Some((s, s)))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibs: Stream[Int] = {
    def next(f0: Int, f1: Int): Stream[Int] = cons(f0, next(f1, f0 + f1))

    next(0, 1)
  }

  def fibsUnfold: Stream[Int] = unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => Empty
  }
}
