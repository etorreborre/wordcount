package essays

import org.specs2._
import org.parboiled.scala._
import parserunners.ReportingParseRunner
import ParboiledTextParsing.useRule1

class ParboiledTextParsingSpec extends Specification with ParboiledTextParsing { def is  =

"Parsers"                                                                                                                         ^
  { pages.parse("p.3") === Pages("p.3") }                                                                                         ^
  { pages.parse("p.34") === Pages("p.34") }                                                                                       ^
  { pages.parse("p34") === Pages("p34") }                                                                                         ^
  { pages.parse("p 34") === Pages("p 34") }                                                                                       ^
  { pages.parse("p.215/324") === Pages("p.215/324") }                                                                             ^
  { pages.parse("pp.215-324") === Pages("pp.215-324") }                                                                           ^
  { pages.parse("pp.215/324") === Pages("pp.215/324") }                                                                           ^
                                                                                                                                  p^
"Years"                                                                                                                           ^
  { years.parse("1905") === Years(Year(1905)) }                                                                                   ^
  { years.parse("1905-1910") === Years(Year(1905), Some("-"), Some(Year(1910))) }                                                       ^
  { years.parse("1905/1910") === Years(Year(1905), Some("/"), Some(Year(1910))) }                                                       ^
  { years.parse("[1905]-1910") === Years(Year(1905, firstEdition=true), Some("-"), Some(Year(1910))) }                                  ^
  { years.parse("1905 1910") === Years(Year(1905), Some(" "), Some(Year(1910))) }                                                       ^
                                                                                                                                  end


}

object ParboiledTextParsing extends ParboiledTextParsing { outer =>
  implicit def useRule1[T](rule: Rule1[T]): ReportingParserRunnerRule1[T] = new ReportingParserRunnerRule1[T](rule)

  class ReportingParserRunnerRule1[T](rule: Rule1[T]) {
    def parse(input: String) = outer.parse(rule, input)
  }
  def parse[T](rule: Rule1[T], input: String) = ReportingParseRunner(rule).run(input).result.getOrElse(Pages("wrong input"))
}

trait ParboiledTextParsing extends org.parboiled.scala.Parser {

  // simple parsers
  def space  = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  def digit  = rule { "0" - "9" }
  def digits = rule { oneOrMore(digit) }

  // pages
  def pagePrefix = rule { ("pp" | "p") ~ optional(".") ~ space }
  def page       = rule { group(pagePrefix ~ digits) ~> Pages }
  def pageSep    = rule { optional(anyOf("/-")) }
  def pageRange  = rule { group(pagePrefix ~ digits ~ pageSep ~ space ~ digits) ~> Pages }
  def pages      = pageRange | page

  // years
  def firstEdition = rule { "[" ~ nTimes(4, digit) ~> ((s: String) => Year(s.toInt, firstEdition = true)) ~ "]" }
  def laterEdition = rule { nTimes(4, digit) ~> ((s: String) => Year(s.toInt)) }

  def year    = rule { firstEdition | laterEdition }
  def yearSep = rule { group(anyOf("/- ") ~ space) ~> ((s: String) => s ) }
  def years   = rule { year ~ optional(yearSep) ~ optional(year) ~~> Years }

  def letter = rule { "a" - "z" }
  def word  = rule { zeroOrMore(letter) ~> ((s: String) => Results(Words.fromString(s)))}
  def words = rule { zeroOrMore(word) }
  def referencedText = rule { words }

}
