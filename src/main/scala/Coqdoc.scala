package org.proofcafe.coqdoc

object Coqdoc {
  def markdowns(elements : List[{ def toMarkdown : String}]) =
    elements.map{ _.toMarkdown }.mkString("")

  def indent(str : String) =
    str.lines.map("    " + _).mkString("\n")
}

/*******************************
 * block element
 ******************************* */
abstract class Coqdoc {
  def toMarkdown : String
}

case class Doc(elements : List[Element]) extends Coqdoc {
  def toMarkdown =
    Coqdoc.markdowns(elements)
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
abstract class Block  extends Element
abstract class Inline extends Element

/*
 * [[
 *   foo bar baz
 * ]]
 *
 */
case class BlockCode(value : String) extends Block {
  def toMarkdown =
    Coqdoc.indent(value)
}

/*
 * heading
 */
object Head {
  var first : Boolean = true
}

case class Head(level : Int, title : List[Inline]) extends Block {
  def toMarkdown = {
    val n = if(Head.first) {
      Head.first = false
      level
    } else {
      level + 1
    }
    ("#" * n) + " " + Coqdoc.markdowns(title)
  }
}

case class Ul(xs : List[Li]) extends Block {
  def toMarkdown =
    Coqdoc.markdowns(xs)
}

case class Li(elements : List[Inline]) {
  def toMarkdown = " * %s".format(Coqdoc.markdowns(elements))
}

case class Verbatim(text : String) extends Block {
  def toMarkdown =
    Coqdoc.indent(text)
}

case class Paragraph(elements : List[Inline]) extends Block {
  def toMarkdown = Coqdoc.markdowns(elements)
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
