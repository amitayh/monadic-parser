package amitayh

import amitayh.Parser.pure

object BasicParsers {

  def parseWith[A](filter: Char => Boolean,
                   transform: String => A): Parser[A] = str => {
    val matching = str.takeWhile(filter)
    if (matching.isEmpty) None
    else Some(transform(matching), str.drop(matching.length))
  }

  val parseInt: Parser[Int] = parseWith(_.isDigit, _.toInt)

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

  val parseWhitespace: Parser[Int] =
    parseWith(_.isWhitespace, _.length) orElse pure(0)

  def parseCharIgnoreWhitespace(expected: Char): Parser[Unit] = for {
    _ <- parseWhitespace
    _ <- parseChar(expected)
    _ <- parseWhitespace
  } yield ()

  def parseTwoOrMore[A](item: Parser[A], separator: Parser[_]): Parser[List[A]] = for {
    head <- item
    _ <- separator
    tail <- parseOneOrMore(item, separator)
  } yield head :: tail

  def parseOneOrMore[A](item: Parser[A], separator: Parser[_]): Parser[List[A]] =
    parseTwoOrMore(item, separator) orElse item.map(List(_))

  def parseZeroOrMore[A](item: Parser[A], separator: Parser[_]): Parser[List[A]] =
    parseOneOrMore(item, separator) orElse pure(Nil)

}
