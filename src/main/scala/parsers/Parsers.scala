package org.iyunbo.coding
package parsers

import pbt.Prop.forAll
import pbt.{Gen, Prop, SGen}

import org.iyunbo.coding.pbt.Gen.S.**

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] {
  self =>
  // primitive
  def run[A](p1: Parser[A])(input: String): Either[ParseError, A]
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  // primitive
  def slice[A](p1: Parser[A]): Parser[String]
  // primitive
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p1)(a => map(p2)(b => (a, b)))

  def many[A](p1: Parser[A]): Parser[List[A]] =
    map2(p1, many(p1))(_ :: _) | succeed(List())
  def many1[A](p1: Parser[A]): Parser[List[A]] = map2(p1, many(p1))(_ :: _)
  def listOfN[A](n: Int, p1: Parser[A]): Parser[List[A]] = n match {
    case _ <= 0 => succeed(List())
    case i      => map2(p1, listOfN(i - 1, p1))(_ :: _)
  }
  def map[A, B](p1: Parser[A])(f: A => B): Parser[B] =
    p1 flatMap (a => succeed(f(a)))
  // primitive
  def flatMap[A, B](p1: Parser[A])(f: A => Parser[B]): Parser[B]
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(
      f: (A, B) => C
  ): Parser[C] =
    p1 ** p2 map f

  // primitive
  implicit def regex(r: Regex): Parser[String]

  implicit def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  // primitive
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit
      f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  case class StringParser(value: String) extends Parser[String]
  case class IntParser(value: Int) extends Parser[Int]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  case class Location(input: String, offset: Int = 0) {
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1        => offset + 1
      case lineStart => offset - lineStart
    }
  }

  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String

  val charCount: Parser[Int] = char('a').many.map(_.size)
  val digitFollowedByLetters: Parser[List[Char]] =
    "[0-9]".r.slice.flatMap(s => char('a').listOfN(s.toInt))

  case class ParserOps[A](p1: Parser[A]) {
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p1, p2)
    def |[B >: A](p2: Parser[B]): Parser[B] = p1 or p2
    def many: Parser[List[A]] = self.many(p1)
    def many1: Parser[List[A]] = self.many1(p1)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p1, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = product(p2)
    def map[B](f: A => B): Parser[B] = self.map(p1)(f)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] =
      self.map2(p1, p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p1)(f)
    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p1)
    def slice: Parser[String] = self.slice(p1)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p)(s) == Right(s))

    def labelLaw[A](p: Parser[A], inputs: Gen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _       => true
        }
      }
  }

}

object Parsers {
  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice
    ???
  }
}

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}
