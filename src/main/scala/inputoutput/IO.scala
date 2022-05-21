package org.iyunbo.coding
package inputoutput

import monad.Monad

import scala.io.StdIn

sealed trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] = new IO[B] {
    override def run: B = f(self.run)
  }

  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    override def run: B = f(self.run).run
  }

}

object IO extends Monad[IO] {
  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

  override def unit[A](a: => A): IO[A] = new IO[A] {
    override def run: A = a
  }

  def apply[A](a: => A): IO[A] = unit(a)

  def readLine: IO[String] = IO { StdIn.readLine }

  def printLine(s: String): IO[Unit] = IO { println(s) }
}
