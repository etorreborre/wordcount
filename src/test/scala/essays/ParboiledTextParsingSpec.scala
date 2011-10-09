package essays

import org.specs2._
import org.parboiled._
import scala.parserunners.ReportingParseRunner
import scala.rules.{Rule2, Rule1}
import support.Characters
import ParboiledTextParsing.useRule1

class ParboiledTextParsingSpec extends Specification with ParboiledTextParsing { def is  =

"Parsers"                                                                                                                        ^
  { page.parse("p.3") === Pages("p.3") }                                                                                         ^
                                                                                                                                 end


}

object ParboiledTextParsing extends ParboiledTextParsing { outer =>
  implicit def useRule1[T](rule: Rule1[T]): ReportingParserRunnerRule1[T] = new ReportingParserRunnerRule1[T](rule)

  class ReportingParserRunnerRule1[T](rule: Rule1[T]) {
    def parse(input: String) = outer.parse(rule, input)
  }
  def parse[T](rule: Rule1[T], input: String) = ReportingParseRunner(rule).run(input).result.get
}

trait ParboiledTextParsing extends org.parboiled.scala.Parser {

  def digit  = rule { "0" - "9" }
  def digits = rule { oneOrMore(digit) }
  
  def pagePrefix         = rule { str("p") ~ optional(".") ~> ((s: String) => s) }
  def page: Rule1[Pages] = rule { pagePrefix ~ digits ~> { (p: String, d: String) => Pages(p+d) } }

  def letter = rule { "a" - "z" }
  def word  = rule { zeroOrMore(letter) ~> ((s: String) => Results(Words.fromString(s)))}
  def words = rule { zeroOrMore(word) }
  def referencedText = rule { words }

}
