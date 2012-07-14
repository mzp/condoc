package org.proofcafe.condoc
import scala.util.parsing.combinator._

abstract class Coqdoc
case class Doc(value : String) extends Coqdoc
case class Comment(value : String) extends Coqdoc
case class Code(value : String) extends Coqdoc

object CoqdocParser extends RegexParsers {
  def any : Parser[Char] =
    ".".r ^^ { case s => s.charAt(0) }

  def until(token : Parser[String]) : Parser[String] =
    rep1((not(token)~>any)) ^^ { case cs =>
      cs.mkString
    }

  def comment : Parser[Comment] =
    "(*" ~> until("*)") <~ "*)" ^^ { case str =>
      Comment(str)
    }

  def doc : Parser[Doc] =
    "(**" ~> until("*)") <~ "*)" ^^ { case str =>
      Doc(str)
    }

  def code : Parser[Code] =
    until("(*") ^^ { case str =>
      Code(str)
    }

  def coqdoc : Parser[List[Coqdoc]] =
    rep(doc | comment | code)
}

object Main {
  def main(args : Array[String]) : Unit = {
    println(CoqdocParser.parseAll(CoqdocParser.coqdoc, "(** * foo *) foo (* bug *)"))
  }
}
