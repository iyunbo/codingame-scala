package org.iyunbo.coding
package parsers

class Parser[+T]

case class StringParser(value: String) extends Parser[String]

case class IntParser(value: Int) extends Parser[Int]

object Parsers {
  self =>

  def decode[A](p: Parser[A])(input: String): Either[String, A] = ???

  implicit def char(c: Char): Parser[String] = StringParser(c.toString)

  implicit def string(s: String): Parser[String] = StringParser(s)

  implicit def count(c: Char, s: String): Parser[Int] = ???

  implicit def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = ???

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???

  case class ParserOps[T](p: Parser[T]) {
    def |[B >: T](p2: Parser[B]): Parser[B] = self.or(p, p2)
  }
}
