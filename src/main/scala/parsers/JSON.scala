package org.iyunbo.coding
package parsers

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser(P: Parsers): P.Parser[JSON] = {
    // we'll hide the string implicit conversion and promote strings to tokens instead
    // this is a bit nicer than having to write token everywhere
    import P.{string => _, _}
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    def array: Parser[JArray] = surround("[", "]")(
      value sep "," map (vs => JArray(vs.toIndexedSeq))
    ) scope "array"

    def obj: Parser[JObject] = surround("{", "}")(
      keyVal sep "," map (kvs => JObject(kvs.toMap))
    ) scope "object"

    def keyVal: Parser[(String, JSON)] = escapedQuoted ** (":" *> value)

    def lit: Parser[JSON] = scope("literal") {
      "null".as(JNull) |
        double.map(JNumber) |
        escapedQuoted.map(JString) |
        "true".as(JBool(true)) |
        "false".as(JBool(false))
    }

    def value: Parser[JSON] = lit | obj | array

    root(whitespace *> (obj | array))
  }
}

/** JSON parsing example.
  */
object JSONExample extends App {
  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""

  def printResult(e: Result[JSON]): Unit = println(e)

  val parsing = new Parsing
  val json = JSON.jsonParser(parsing)
  printResult { parsing.run(json)(jsonTxt) }
  println("--")
  printResult { parsing.run(json)(malformedJson1) }
  println("--")
  printResult { parsing.run(json)(malformedJson2) }
}
