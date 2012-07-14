package org.proofcafe.condoc
import scala.util.parsing.combinator._
import java.io.FileReader

// inline
abstract class Element {
  def toMarkdown : String
}
case class Text(value : String) extends Element {
  def toMarkdown = value
}
case class InlineCode(value : String) extends Element {
  def toMarkdown = "`%s`".format(value)
}

case object Qed extends Element {
  def toMarkdown = "☐"
}

case class Comment(value : String) extends Element {
  def toMarkdown = "(* %s *)".format(value)
}

case class Head(level : Int, title : List[Element]) extends Element{
  def toMarkdown =
    "\n" + ("#" * level) + " " + title.map(_.toMarkdown).mkString("")
}

abstract class Coqdoc {
  def toMarkdown : String
}

case class Doc(elements : List[Element]) extends Coqdoc {
  def toMarkdown =
    elements.map(_.toMarkdown).mkString("")
}

case class Code(value : String) extends Coqdoc {
  def toMarkdown =
    value.lines.map("    " + _).mkString("\n")
}

object CoqdocParser extends RegexParsers {
  override def skipWhitespace= false

  def any : Parser[Char] =
    elem("any", { c =>
      c.toInt != 26})

  def head : Parser[Head] =
    rep1("*") ~ elements ^^ { case mark~title =>
      Head(mark.length, title)
    }

  def qed : Parser[Element] =
    "[]" ^^ { case _ => Qed }

  def inlineCode : Parser[InlineCode] =
    "[" ~> until("]") <~ "]" ^^ { case code =>
      InlineCode(code)
  }

  def text : Parser[Text] =
    any ^^ { (c : Char) => Text(c.toString) }

  def comment : Parser[Comment] =
    "(*" ~> until("*)") <~ "*)" ^^ { case str =>
      Comment(str)
    }

  def element : Parser[Element] = head | qed | comment | inlineCode | text

  def uniq(xs : List[Element]) : List[Element] = {
    xs match {
    case List() => List()
    case List(x) => List(x)
    case Text(x) :: Text(y) :: ys =>
      uniq(Text(x+y) :: ys)
    case x :: ys =>
      x :: uniq(ys)
    }
  }

  def elements : Parser[List[Element]] = rep(element) ^^ {case xs => uniq(xs) }

  def until(token : Parser[String]) : Parser[String] =
    rep1((not(token)~>any)) ^^ { case cs =>
      cs.mkString
    }




  def doc : Parser[Doc] =
    "(**" ~> until("*)") <~ "*)" ^^ { case str =>
      parseAll(elements, str) match {
      case Success(es,_) =>
        Doc(es)
      case _ =>
        throw new Exception("error")
    }
  }

  def code : Parser[Code] =
    until("(**") ^^ { case str =>
      Code(str)
    }

  def coqdoc : Parser[List[Coqdoc]] =
    rep(doc | code)
}

object Main {
  def strip(e : Coqdoc) =
    e match {
    case Code(x) =>
      ! x.trim.isEmpty
    case _ =>
      true
  }

  def main(args : Array[String]) : Unit = {
    for(arg <- args) {
      val reader = new FileReader(arg)
      CoqdocParser.parseAll(CoqdocParser.coqdoc, reader) match {
        case CoqdocParser.Success(docs,_) =>
          for(doc <- docs; if strip(doc)) {
            println(doc.toMarkdown)
            println("\n")
          }
      }
    }
  }
}
