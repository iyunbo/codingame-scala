package org.iyunbo.coding
package circe

import io.circe
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json, ParsingFailure}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

sealed trait Foo

case class Bar(xs: Vector[String]) extends Foo

case class Qux(i: Int, d: Option[Double]) extends Foo


class CirceTest extends AnyFlatSpec with should.Matchers {

  it should "test circe integration" in {
    val foo: Foo = Qux(13, Some(14.0))

    val json: String = foo.asJson.spaces2
    println(json)

    val decodedFoo: Either[circe.Error, Foo] = decode[Foo](json)
    println(decodedFoo)
  }

  it should "parse json string" in {
    val rawJson: String =
      """
      {
      "foo": "bar",
      "baz": 123,
      "list of stuff": [ 4, 5, 6 ]
      }
      """
    val parseResult: Either[ParsingFailure, Json] = parse(rawJson)

    println(parseResult)
  }

  it should "manage good error message" in {
    val badJson: String = "yolo"
    // badJson: String = "yolo"

    val result = parse(badJson)

    println(result)
  }

  it should "traverse json tree" in {
    val json: String =
      """
      {
        "id": "c730433b-082c-4984-9d66-855c243266f0",
        "name": "Foo",
        "counts": [1, 2, 3],
        "values": {
          "bar": true,
          "baz": 100.001,
          "qux": ["a", "b"]
        }
      }
      """

    val doc: Json = parse(json).getOrElse(Json.Null)

    val cursor: HCursor = doc.hcursor

    val baz: Decoder.Result[Double] =
      cursor.downField("values").downField("baz").as[Double]

    baz.getOrElse(0) should be(100.001)
  }

}
