package org.proofcafe.coqdoc

object Coqdoc {
  def markdowns(elements : List[{ def toMarkdown : String}], sep : String = "") =
    elements.map{ _.toMarkdown }.mkString(sep)

  def indent(str : String, count : Int = 4) =
    str.lines.map((" " * count) + _).mkString("\n")
}

/*******************************
 * block element
 ******************************* */
abstract class Coqdoc {
  def toMarkdown : String
}

case class Doc(elements : List[Block]) extends Coqdoc {
  def toMarkdown =
    Coqdoc.markdowns(elements,"\n\n")
}

case class Code(code : String) extends Coqdoc {
  def toMarkdown =
    Coqdoc.indent(code)
}

/*******************************
 * element in coqdoc
 ******************************* */
abstract class Element {
  def toMarkdown : String
}
abstract class Block  extends Element {
  def markdown : String
  def toMarkdown =
    markdown
}
abstract class Inline extends Element

/*
 * [[
 *   foo bar baz
 * ]]
 *
 */
case class BlockCode(value : String) extends Block {
  def markdown =
    Coqdoc.indent(value)
}

/*
 * heading
 */
object Head {
  var first : Boolean = true
}

case class Head(level : Int, title : List[Inline]) extends Block {
  def markdown = {
    val n = if(Head.first) {
      Head.first = false
      level
    } else {
      level + 1
    }
    ("#" * n) + " " + Coqdoc.markdowns(title)
  }
}

case class Ul(xs : List[ListItem]) extends Block {
    def markdown = {
      val isInline = xs.forall { x =>
        x match {
          case ListItem(List(Paragraph(_))) => true
          case _ => false
        }
      }
      val sep = if(isInline) "\n" else "\n\n"
      "\n" + Coqdoc.markdowns(xs,sep)
    }
}

case class ListItem(xs : List[Block]) extends Block {
  def markdown = {
    val text = Coqdoc.indent(Coqdoc.markdowns(xs, "\n"), 3).trim
    " * %s".format(text)
  }
}

case class Verbatim(text : String) extends Block {
  def markdown =
    Coqdoc.indent(text)
}

case class Paragraph(elements : List[Inline]) extends Block {
  def markdown = Coqdoc.markdowns(elements)
}

case object Blank extends Block {
  override def toMarkdown = ""
  def markdown = ""
}

// comment
case class Comment(value : String) extends Inline {
  // output only if ""FILL IN HERE"
  def toMarkdown =
    if(value.trim == "FILL IN HERE")
      "(* %s *)".format(value)
    else
      ""
}

// [code]
case class InlineCode(value : String) extends Inline {
  def toMarkdown = "`%s`".format(value)
}

case object Qed extends Inline {
  def toMarkdown = "☐\n"
}

case class Text(value : String) extends Inline {
  def toMarkdown = value.lines.map{_.trim}.mkString("\n")
}
