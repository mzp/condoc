package org.proofcafe.coqdoc
import scala.util.parsing.combinator._
import java.io.FileReader
import java.io.FileWriter

object Main {
  def use[T <: { def close() : Unit}](resource : T)(action : T => Unit) {
    try{
      action(resource)
    } finally{
      resource.close()
    }
  }

  def main(args : Array[String]) : Unit = {
    for(arg <- args) {
      use(new FileReader(arg)) { reader =>
        use(new FileWriter("dump")) { w =>
          CoqdocParser.parseAll(CoqdocParser.coqdoc, reader) match {
            case CoqdocParser.Success(docs,_) =>
              for(doc <- docs) {
                w.write(doc.toString+"\n")
                println(doc.toMarkdown)
                println("")
              }
          } } } }
  }
}
