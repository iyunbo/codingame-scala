package org.iyunbo.coding
package parsers

import pbt.Prop.forAll
import pbt.{Gen, Prop}

import java.util.regex.Pattern
import scala.util.matching.Regex

trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e, isCommitted) => Failure(f(e), isCommitted)
    case _                       => this
  }

  def uncommit: Result[A] = this match {
    case Failure(e, true) => Failure(e, isCommitted = false)
    case _                => this
  }

  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(e, c) => Failure(e, c || isCommitted)
    case _             => this
  }

  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a, m) => Success(a, m + n)
    case _             => this
  }
}
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError, isCommitted: Boolean = true)
    extends Result[Nothing]

trait Parsers {
  self =>
  type Parser[+A] = String => Result[A]
  // primitive
  def run[A](p1: Parser[A])(input: String): Result[A]
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  def fail[A]: ParseError
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
    case e if e <= 0 => succeed(List())
    case i           => map2(p1, listOfN(i - 1, p1))(_ :: _)
  }
  def map[A, B](p1: Parser[A])(f: A => B): Parser[B] =
    p1 flatMap (a => succeed(f(a)))
  // primitive
  def flatMap[A, B](p1: Parser[A])(f: A => Parser[B]): Parser[B]
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(
      f: (A, B) => C
  ): Parser[C] =
    p1 ** p2 map (x => f(x._1, x._2))

  def advanceBy(input: String, n: Int): String = input.substring(n)

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

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def errorLocation(e: ParseError): Location =
    e.latestLoc.getOrElse(Location(""))
  def errorMessage(e: ParseError): String =
    e.latest map (_._2) getOrElse "<empty message>"

  val charCount: Parser[Int] = char('a').many.map(_.size)
  val digitFollowedByLetters: Parser[List[Char]] =
    "[0-9]".r.slice.flatMap(s => char('a').listOfN(s.toInt))

  /** Sequences two parsers, ignoring the result of the first.
    * We wrap the ignored half in slice, since we don't care about its result.
    */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  /** Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result.
    */
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, _) => a)

  def opt[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) or succeed(None)

  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = "\\s*".r

  /** Parser which consumes 1 or more digits. */
  def digits: Parser[String] = "\\d+".r

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
    // rather annoying to write, left as an exercise
    // we'll just use quoted (unescaped literals) for now
    token(quoted label "string literal")

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep[A](
      p: Parser[A],
      p2: Parser[Any]
  ): Parser[
    List[A]
  ] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p, p2) or succeed(List())

  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  /** Parses a sequence of left-associative binary operators with the same precedence. */
  def opL[A](p: Parser[A])(op: Parser[(A, A) => A]): Parser[A] =
    map2(p, many(op ** p))((h, t) => t.foldLeft(h)((a, b) => b._1(a, b._2)))

  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(
      p: => Parser[A]
  ): Parser[A] =
    start *> p <* stop

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

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

    def label(msg: String): Parser[A] = self.label(msg)(p1)

    def scope(msg: String): Parser[A] = self.scope(msg)(p1)

    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p1, p2)
    def <*(p2: => Parser[Any]): Parser[A] = self.skipR(p1, p2)
    def token: Parser[A] = self.token(p1)
    def sep(separator: Parser[Any]): Parser[List[A]] = self.sep(p1, separator)
    def sep1(separator: Parser[Any]): Parser[List[A]] = self.sep1(p1, separator)
    def as[B](b: B): Parser[B] = self.map(self.slice(p1))(_ => b)
    def opL(op: Parser[(A, A) => A]): Parser[A] = self.opL(p1)(op)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s =>
        run(p)(s) match {
          case Success(_, _) => true
          case Failure(_, _) => false
        }
      )

    def labelLaw[A](p: Parser[A], inputs: Gen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Failure(e, _) => errorMessage(e) == msg
          case _             => true
        }
      }

    def attemptLaw[A](p2: Parser[A]): Prop =
      forAll(Gen.string) { s =>
        (attempt(string(s) map (_ => fail)) or p2) == p2
      }
  }

}

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError = copy((loc, msg) :: stack)

  def latest: Option[(Location, String)] = stack.lastOption

  def latestLoc: Option[Location] = latest map (_._1)

  def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)
}
case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1        => offset + 1
    case lineStart => offset - lineStart
  }
  def advanceBy(n: Int): Location = copy(offset = offset + n)
}

class Parsing extends Parsers {
  override def run[A](p1: Parser[A])(input: String): Result[A] = p1(input)

  override def fail[A]: ParseError = ParseError(List())

  override def slice[A](p1: Parser[A]): Parser[String] = s =>
    p1(s) match {
      case Success(_, n) => string(advanceBy(s, n))(s)
      case f: Failure    => f
    }

  override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = s =>
    p1(s) match {
      case Failure(_, false) => p2(s)
      case r                 => r
    }

  override def flatMap[A, B](p1: Parser[A])(f: A => Parser[B]): Parser[B] = s =>
    p1(s) match {
      case Success(a, n) =>
        f(a)(advanceBy(s, n)).addCommit(n != 0).advanceSuccess(n)
      case f: Failure => f
    }

  override implicit def regex(r: Regex): Parser[String] = {
    case ok if r.matches(ok) => Success(ok, ok.length)
    case _                   => Failure(fail)
  }

  override implicit def string(s: String): Parser[String] =
    scope(s"expected string: $s")({
      case ok if ok.equals(s) => Success(s, s.length)
      case _                  => Failure(fail)
    })

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = s =>
    p(s).mapError(_.label(msg))

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = s =>
    p(s).mapError(err => err.push(errorLocation(err), msg))

  override def attempt[A](p: Parser[A]): Parser[A] = s => p(s).uncommit

}
