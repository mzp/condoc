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
    rep(inline) map { xs =>
      format(xs) map {
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

  def head : Parser[Head] =
    """ *\*""".r ~> rep("*") ~ until("\n") ^^ { case mark~title =>
      Head(mark.length+1, InlineParser.parse(title))
    }

  def blockCode : Parser[BlockCode] =
    """ *\[\[""".r ~> until("]]") <~ "]]" ^^ { case code =>
      BlockCode(code)
    }

  def verbatim : Parser[Verbatim] =
    " *<<".r ~> until(">>") <~ ">>" ^^ { case text =>
      Verbatim(text)
    }

  def oneline =
    ".*".r

  var listIndent = 0

  def listMarker =
    "^ *- *".r ^^ { case mark =>
      val n = mark.takeWhile(' ' == _).length
      listIndent =  n
      mark
  }

  def ul : Parser[Ul] =
    listMarker ~ oneline ~ opt("\n") ~ rep(indentedLine) ^^ { case header ~ str ~ _ ~ xs =>
      val text = str + xs.mkString("\n")
      val blocks = parse(text)
      Ul(List(ListItem(blocks)))
    }

  def indentedLine : Parser[String] = Parser { input =>
    val indent = " " * (listIndent + 1)
    var parser = (indent ~> oneline <~ opt("\n")) | blank ^^ { _ => "" }
    parser(input)
  }

  def paragraph : Parser[Paragraph] =
    ".+".r <~ opt("\n") ^^ { case line =>
      Paragraph(InlineParser.parse(line.trim))
    }

  def blank = "\n" ^^ { _ =>
    Blank
  }

  def format(xs : List[Block]) : List[Block] = {
    xs match {
      case List() =>
        List()
      case List(x) =>
        List(x)
      case Paragraph(x) :: Paragraph(y) :: ys =>
        format(Paragraph(InlineParser.format(x ++ y)) :: ys)
      case Ul(x) :: Ul(y) :: ys =>
        format(Ul(x ++ y) :: ys)
      case x :: ys =>
        x :: format(ys)
    }
  }

  def block : Parser[Block] = head | ul | blockCode | verbatim | paragraph | blank

  def blocks : Parser[List[Block]] = rep(block) ^^ {case xs =>
    format(xs) filter {
      case Paragraph(List()) => false
      case Blank => false
      case _ => true
    }
  }

  def parse(str : String) : List[Block] = {
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

  def format(xs : List[Coqdoc]) : List[Coqdoc] =
    xs match {
      case List() =>
        List()
      case List(x) =>
        List(x)
      case Doc(x) :: Doc(y) :: ys =>
        format(Doc(x ++ y) :: ys)
      case Code(x) :: Code(y) :: ys =>
        format(Code(x ++ y) :: ys)
      case x :: ys =>
        x :: format(ys)
    }

  def coqdoc : Parser[List[Coqdoc]] =
    rep(doc | code) ^^ { xs => format(xs filter { doc =>
      doc match {
        case Code(x) =>
          ! x.trim.isEmpty
        case _ =>
          true
      } }) }

  def parse(str : String) =
    parseWith(coqdoc)(str)
}
