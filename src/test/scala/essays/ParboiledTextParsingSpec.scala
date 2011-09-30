package essays

import org.specs2._

class ParboiledTextParsingSpec extends Specification with ParboiledTextParsing { def is  =

"Parsers"                                                                                                                        ^
                                                                                                                                 end

}

object ParboiledTextParsing extends ParboiledTextParsing

trait ParboiledTextParsing extends org.parboiled.scala.Parser {

  def word = rule { zeroOrMore(anyOf("a-z")) ~> ((s: String) => Results(Words.fromString(s)))}
  def words = rule { zeroOrMore(word) }
  def referencedText = rule { words }

}
