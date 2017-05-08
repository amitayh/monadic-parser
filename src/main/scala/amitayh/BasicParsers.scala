package amitayh

import scala.collection.immutable.Seq

object BasicParsers {

  val parseInt = new Parser[Int] {
    override def apply(str: String): Option[(Int, String)] = {
      val digits = str.takeWhile(_.isDigit)
      if (digits.isEmpty) None
      else Some(digits.toInt, str.drop(digits.length))
    }
  }

  def parseChar(expected: Char) = new Parser[Char] {
    override def apply(str: String): Option[(Char, String)] = {
      if (str.head != expected) None
      else Some(str.head, str.tail)
    }
  }

  def parseString(expected: String) = new Parser[String] {
    override def apply(str: String): Option[(String, String)] = {
      if (!str.startsWith(expected)) None
      else Some(expected, str.drop(expected.length))
    }
  }

  def parseOneOf(options: String*): Parser[String] =
    options.map(parseString).reduce(_ orElse _)

  def parseStringUntil(end: Char) = new Parser[String] {
    override def apply(str: String): Option[(String, String)] = {
      str.indexOf(end) match {
        case -1 => Some(str, "")
        case endPosition => Some(str.take(endPosition), str.drop(endPosition))
      }
    }
  }

  def constant[A](value: A) = new Parser[A] {
    override def apply(str: String): Option[(A, String)] = Some(value, str)
  }

  val parseWhitespace = new Parser[Int] {
    override def apply(str: String): Option[(Int, String)] = {
      val whitespace = str.takeWhile(_.isWhitespace).length
      Some(whitespace, str.drop(whitespace))
    }
  }

  def parseCharIgnoreWhitespace(expected: Char): Parser[Unit] = for {
    _ <- parseWhitespace
    _ <- parseChar(expected)
    _ <- parseWhitespace
  } yield ()

  def parseMany[A](parser: Parser[A],
                   separator: Char = ',',
                   values: Seq[A] = Vector.empty): Parser[Seq[A]] = {
    parser.flatMap { value =>
      val allValues = values :+ value
      parseCharIgnoreWhitespace(separator)
        .flatMap(_ => parseMany(parser, separator, allValues))
        .orElse(constant(allValues))
    } orElse constant(values)
  }

}
