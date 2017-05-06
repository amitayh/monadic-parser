package amitayh

trait Parser[+A] extends (String => Option[(A, String)]) { outer =>
  def flatMap[B](f: A => Parser[B]): Parser[B] = new Parser[B] {
    override def apply(str: String): Option[(B, String)] = outer(str).flatMap {
      case (value, rest) => f(value)(rest)
    }
  }

  def map[B](f: A => B): Parser[B] = new Parser[B] {
    override def apply(str: String): Option[(B, String)] = outer(str).map {
      case (value, rest) => (f(value), rest)
    }
  }

  def orElse[B >: A](alternative: => Parser[B]): Parser[B] = new Parser[B] {
    override def apply(str: String): Option[(B, String)] =
      outer(str) orElse alternative(str)
  }
}
