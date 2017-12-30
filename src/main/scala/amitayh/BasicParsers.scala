package amitayh

import amitayh.Parser.pure

import scala.collection.immutable.Seq

object BasicParsers {

  val parseInt: Parser[Int] = str => {
    val digits = str.takeWhile(_.isDigit)
    if (digits.isEmpty) None
    else Some(digits.toInt, str.drop(digits.length))
  }

  def parseChar(expected: Char): Parser[Char] = str => {
    if (str.head != expected) None
    else Some(str.head, str.tail)
  }

  def parseString(expected: String): Parser[String] = str => {
    if (!str.startsWith(expected)) None
    else Some(expected, str.drop(expected.length))
  }

  def parseOneOf(options: String*): Parser[String] =
    options.map(parseString).reduce(_ orElse _)

  def parseStringUntil(end: Char): Parser[String] = str => {
    str.indexOf(end) match {
      case -1 => Some(str, "")
      case endPosition => Some(str.take(endPosition), str.drop(endPosition))
    }
  }

  val parseWhitespace: Parser[Int] = str => {
    val whitespace = str.takeWhile(_.isWhitespace).length
    Some(whitespace, str.drop(whitespace))
  }

  def parseCharIgnoreWhitespace(expected: Char): Parser[Unit] = for {
    _ <- parseWhitespace
    _ <- parseChar(expected)
    _ <- parseWhitespace
  } yield ()

  def parseTwoOrMore[A](item: Parser[A], separator: Parser[_]): Parser[Seq[A]] = for {
    first <- item
    _ <- separator
    rest <- parseZeroOrMore(item, separator)
  } yield first +: rest

  def parseOne[A](item: Parser[A]): Parser[Seq[A]] = item.map(Vector(_))

  def parseZeroOrMore[A](item: Parser[A], separator: Parser[_]): Parser[Seq[A]] =
    parseTwoOrMore(item, separator) orElse
      parseOne(item) orElse
      pure(Vector.empty)

}
