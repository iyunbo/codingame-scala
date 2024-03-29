package org.iyunbo.coding
package reactive

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B]

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => Option[B]): Option[B]

  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))

  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)

  override def getOrElse[B >: A](default: => B): B = get

  override def orElse[B >: A](ob: => Option[B]): Option[B] = Some(get)

  override def filter(f: A => Boolean): Option[A] = if (f(get)) Some(get) else None
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  override def filter(f: Nothing => Boolean): Option[Nothing] = None
}

object Option {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case List() => Some(List())
      case x :: xs => x.flatMap(a => {
        sequence(xs).flatMap(l => Some(a :: l))
      })
    }
  }

  // a bit later in the chapter we'll learn nicer syntax for
  // writing functions like this
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case List() => Some(List())
    case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)
  }

  def sequenceT[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
}