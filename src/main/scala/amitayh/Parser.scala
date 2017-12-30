package amitayh

import amitayh.Parser.pure

trait Parser[+A] extends (String => Option[(A, String)]) { outer =>

  def flatMap[B](f: A => Parser[B]): Parser[B] = str => outer(str).flatMap {
    case (value, rest) => f(value)(rest)
  }

  def map[B](f: A => B): Parser[B] = flatMap(value => pure(f(value)))

  def orElse[B >: A](alternative: => Parser[B]): Parser[B] = str =>
    outer(str) orElse alternative(str)

}

object Parser {
  def pure[A](value: A): Parser[A] = str => Some(value, str)
}
