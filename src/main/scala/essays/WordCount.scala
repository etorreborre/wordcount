package essays
import java.io.File
import util.parsing.combinator.RegexParsers
import util.parsing.input.{ CharSequenceReader, Reader }
import io.{Codec, Source}
import docs._
import Doc._
import DocReader._
import docs.Doc._

trait WordCounter extends ParboiledTextParsing {

  def count(path: String): Results = countDoc(path)

  def countDoc(doc: Doc): Results =
    doc.read match {
      case Left(e)  => e.printStackTrace(); Results(message = e.getMessage)
      case Right(t) => parse(t) add Results(fullTextWordsNumber = t.wordsNumber)
    }

  def display(results: Results)(args: Array[String]) {
    println("Number of words: "+results.wordsNumber)
    println("References")
    results.references foreach println
  }

}

object WordCounter extends WordCounter

object WordCounterApp extends WordCounter with App {
  override def main(args: Array[String]) {
    if (args.size != 1) println("please enter the file path")
    else if (!new File(args(0)).exists) println("the file "+args(0)+" doesn't exist")
    else display(count(args(0)))(args)
  }
}

