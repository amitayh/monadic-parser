package amitayh

import amitayh.BasicParsers._

import scala.collection.immutable.Seq

object JsonExample extends App {

  sealed trait JsonNode
  case object NullNode extends JsonNode
  case class StringNode(string: String) extends JsonNode
  case class NumberNode(number: Int) extends JsonNode
  case class BooleanNode(boolean: Boolean) extends JsonNode
  case class ArrayNode(values: Seq[JsonNode]) extends JsonNode
  case class ObjectNode(entries: Map[String, JsonNode]) extends JsonNode

  // ------------------------------------------------------------------------

  val parseJsonNull = parseString("null").map(_ => NullNode)

  val parseJsonNumber = parseInt.map(NumberNode)

  val parseJsonBoolean = parseOneOf("true", "false")
    .map(value => BooleanNode(value.toBoolean))

  val parseJsonString = for {
    _ <- parseChar('"')
    str <- parseStringUntil('"')
    _ <- parseChar('"')
  } yield StringNode(str)

  val parseJsonArray = for {
    _ <- parseCharIgnoreWhitespace('[')
    values <- parseMany(parseJson)
    _ <- parseCharIgnoreWhitespace(']')
  } yield ArrayNode(values)

  val parseObjectEntry: Parser[(String, JsonNode)] = for {
    key <- parseJsonString
    _ <- parseCharIgnoreWhitespace(':')
    value <- parseJson
  } yield key.string -> value

  val parseJsonObject = for {
    _ <- parseCharIgnoreWhitespace('{')
    entries <- parseMany(parseObjectEntry)
    _ <- parseCharIgnoreWhitespace('}')
  } yield ObjectNode(entries.toMap)

  lazy val parseJson: Parser[JsonNode] =
    parseJsonNull orElse
      parseJsonNumber orElse
      parseJsonString orElse
      parseJsonBoolean orElse
      parseJsonArray orElse
      parseJsonObject

  // ------------------------------------------------------------------------

  val json = """{"foo": "bar", "baz": [123, true, false], "qux": null}"""

  println(parseJson(json))

}
