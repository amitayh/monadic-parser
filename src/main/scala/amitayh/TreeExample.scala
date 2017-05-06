package amitayh

import amitayh.BasicParsers.{parseChar, parseInt}

object TreeExample extends App {

  case class Tree(value: Int,
                  left: Option[Tree],
                  right: Option[Tree])

  def serialize(tree: Option[Tree]): String = tree match {
    case None => "*"
    case Some(Tree(value, left, right)) =>
      s"$value(${serialize(left)},${serialize(right)})"
  }

  def serialize(tree: Tree): String = serialize(Some(tree))

  def deserialize(str: String): Option[Tree] = parseTree(str).collect {
    case (Some(tree), rest) if rest.isEmpty => tree
  }

  // ------------------------------------------------------------------------

  val parseNoneNode: Parser[Option[Tree]] =
    parseChar('*').map(_ => None)

  val parseSomeNode: Parser[Option[Tree]] = for {
    value <- parseInt
    _ <- parseChar('(')
    left <- parseTree
    _ <- parseChar(',')
    right <- parseTree
    _ <- parseChar(')')
  } yield Some(Tree(value, left, right))

  lazy val parseTree: Parser[Option[Tree]] =
    parseNoneNode orElse parseSomeNode

  // ------------------------------------------------------------------------

  val serialized = "1(2(4(*,*),5(*,*)),3(*,6(*,*)))"

  println(deserialize(serialized))

}
