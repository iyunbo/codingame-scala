package org.iyunbo.coding
package reactive

import reactive.Stream.cons

sealed trait Stream[+A] {
  def toList: List[A]

  def take(n: Int): Stream[A]

  def drop(n: Int): Stream[A]

  def takeWhile(p: A => Boolean): Stream[A] = folderRight(Stream[A]())((a, s) => if (p(a)) cons(a, s) else s)

  def folderRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().folderRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = folderRight(true)((a, s) => p(a) && s)

  def headOption: Option[A] = folderRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = folderRight(Stream[B]())((a, bs) => cons(f(a), bs))

  def filter(p: A => Boolean): Stream[A] = folderRight(Stream[A]())((a, bs) => if (p(a)) cons(a, bs) else bs)

  def append[B >: A](a: => B): Stream[B] = folderRight(Stream(a))((x, bs) => cons[B](x, bs))

  def flatMap[B](f: A => Stream[B]): Stream[B] = folderRight(Stream[B]())((a, bs) => f(a) concat bs)

  def concat[B >: A](other: Stream[B]): Stream[B] = folderRight(other)((a, ss) => cons(a, ss))
}

case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = List()

  override def take(n: Int): Stream[Nothing] = Empty

  override def drop(n: Int): Stream[Nothing] = Empty

}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toList: List[A] = h() :: t().toList

  override def take(n: Int): Stream[A] = if (n <= 0) Empty else Stream.cons(h(), t().take(n - 1))

  override def drop(n: Int): Stream[A] = if (n <= 0) this else t().drop(n - 1)

  //  override def takeWhile(p: A => Boolean): Stream[A] = {
  //    lazy val head = h()
  //    lazy val tail = t()
  //    if (p(head)) Stream.cons(head, tail.takeWhile(p))
  //    else tail.takeWhile(p)
  //  }
}

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
}
