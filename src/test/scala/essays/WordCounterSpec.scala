package essays

import org.specs2._
import WordCounterApp._
import docs._

class WordCounterSpec extends Specification { def is =
                                                                                                                        """
WordCounter counts words from a text document, but only counting normal text, not citations or references

  File => String => References
                                                                                                                        """^
                                                                                                                        p^
  "Count words from a sample file"                                                                                      ! sample^
  "Read files" ~/ new DocReaderSpec                                                                                     ^
  "Parse text" ~/ new TextParsingSpec                                                                                   ^
                                                                                                                        end

  def sample = {
    val result = parse("src/test/resources/hello-world.txt")
    result.wordsNumber aka result.message must >(0)
  }

}