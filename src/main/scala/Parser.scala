package org.proofcafe.coqdoc
import scala.util.parsing.combinator._

trait Util extends RegexParsers {
  def any : Parser[Char] =
    elem("any", { c =>
      c.toInt != 26
    })

  def until(token : Parser[String]) : Parser[String] =
    rep1((not(token)~>any)) ^^ { case cs =>
      cs.mkString("")
    }

  def parseWith[A](parser : Parser[A])(str : String) : A = {
    parseAll(parser, str) match {
      case Success(value, _) =>
        value
      case Failure(reason,_) =>
        throw new RuntimeException(reason)
    }
  }
}

object InlineParser extends RegexParsers with Util{
  override def skipWhitespace= false

  def qed : Parser[Inline] =
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

  def inline =
    qed | inlineCode | comment | text
  def inlines =
    rep(inline) map { format(_) map {
      case Text(s) => Text(s.trim)
      case x => x
    } }

  def format(xs : List[Inline]) : List[Inline] = {
    xs match {
      case List() =>
        List()
      case List(x) =>
        List(x)
      case Text(x) :: Text(y) :: ys =>
        format(Text(x+y) :: ys)
      case x :: ys =>
        x :: format(ys)
    }
  }

  def parse(str : String) =
    parseWith(inlines)(str)
}

object BlockParser extends RegexParsers with Util {

  override def skipWhitespace= false

  def ul : Parser[Ul] =
    rep1(li) ^^ { xs => Ul(xs) }

  def head : Parser[Head] =
    """ *\*""".r ~> rep("*") ~ until("\n") ^^ { case mark~title =>
      Head(mark.length+1, InlineParser.parse(title))
    }

  def blockCode : Parser[BlockCode] =
    "[[" ~> until("]]") <~ "]]" ^^ { case code =>
      BlockCode(code)
    }

  def verbatim : Parser[Verbatim] =
    "<<" ~> until(">>") <~ ">>" ^^ { case text =>
      Verbatim(text)
    }

  def oneline =
    ".*".r

  def li : Parser[Li] =
    "^ *- *".r ~> oneline <~ opt("\n") ^^ { case str =>
      Li(InlineParser.parse(str))
    }

  def paragraph : Parser[Paragraph] =
    ".+".r <~ opt("\n") ^^ { case line =>
      Paragraph(InlineParser.parse(line.trim))
    }

  def blank = "\n" ^^ { _ =>
    Paragraph(List())
  }

  def format(xs : List[Block]) : List[Block] = {
    xs match {
      case List() =>
        List()
      case List(x) =>
        List(x)
      case Paragraph(x) :: Paragraph(y) :: ys =>
        format(Paragraph(x ++ y) :: ys)
      case x :: ys =>
        x :: format(ys)
    }
  }

  def block : Parser[Block] = head | ul | blockCode | verbatim | paragraph | blank

  def blocks : Parser[List[Element]] = rep(block) ^^ {case xs =>
    format(xs) filter {
      case Paragraph(List()) => false
      case _ => true
    }
  }

  def parse(str : String) = {
    parseWith(blocks)(str)
  }
}

object CoqdocParser extends RegexParsers with Util {
  override def skipWhitespace= false

  // parsing nested comment
  def nest(start : String, end : String, n : Int) : Parser[String] = Parser { case input =>
    var level = n
    val anyStr = any ^^ { case c => c.toString }

    val parser = (
        start  ^^ { case s => level += 1; s }
      | Parser{ (input : Input) =>
        (end : Parser[String])(input) match {
          case Success(value, rest) =>
            level -= 1
            if(level < 1) {
              Error("", input)
            } else {
              Success(value, rest)
            }
          case x =>
            x
        }
      }
      | anyStr)
      (rep1(parser) ^^ { xs => xs.mkString("") })(input)
  }

  def doc : Parser[Doc] =
    "(**" ~> nest("(*", "*)", 1) <~ "*)" ^^ { case str =>
      Doc(BlockParser.parse(str))
  }

  def code : Parser[Code] =
    until("(**") ^^ { case str =>
      // hack for FILL IN HERE
      val s = """(?s)\(\*(.*)\*\)""".r.replaceAllIn(str, { m =>
        if(m.group(1) == "FILL IN HERE")
          "(* FILL IN HERE *)"
        else
          ""
      })
      Code(s.trim)
    }

  def format(xs : List[Coqdoc]) =
    xs filter { doc =>
      doc match {
        case Code(x) =>
          ! x.trim.isEmpty
        case _ =>
          true
      }
    }

  def coqdoc : Parser[List[Coqdoc]] =
    rep(doc | code) ^^ { format(_) }

  def parse(str : String) =
    parseWith(coqdoc)(str)
}
