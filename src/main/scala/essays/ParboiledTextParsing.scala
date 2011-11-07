package essays

import org.parboiled.scala._

trait ParboiledTextParsing extends Parser {

  // simple parsers
  def space        = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  def digit        = rule { "0" - "9" }
  def digits       = rule { oneOrMore(digit) }
  def letter       = rule { "a" - "z" | "A" - "Z" }
  def punctuation  = rule { anyOf("!?&.,;:-") }
  def word         = rule { oneOrMore("[" | "]" | letter | digit) ~ optional(punctuation) }
  def words        = rule { oneOrMore(optional(space) ~ word ~ optional(space)) }
  def commaSep     = rule { "," ~ space }

  // pages
  def pagePrefix   = rule { ("pp" | "p") ~ optional(".") ~ space }
  def page         = rule { group(pagePrefix ~ digits) ~> Pages }
  def pageSep      = rule { optional(anyOf("/-")) }
  def pageRange    = rule { group(pagePrefix ~ digits ~ pageSep ~ space ~ digits) ~> Pages }
  def pages        = rule { pageRange | page }

  // years
  def firstEdition = rule { "[" ~ nTimes(4, digit) ~> ((s: String) => Year(s.toInt, firstEdition = true)) ~ "]" }
  def laterEdition = rule { nTimes(4, digit) ~> ((s: String) => Year(s.toInt)) }

  def year         = rule { firstEdition | laterEdition }
  def yearSep      = rule { group(anyOf("/- ") ~ space) ~> ((s: String) => s ) }
  def years        = rule { year ~ optional(yearSep) ~ optional(year) ~~> Years }

  // references
  def commaYears   = rule { commaSep ~ years }
  def ref          = rule { oneOrMore(!commaYears ~ ANY) ~> identity ~ commaYears ~ optional(commaSep ~ pages) ~~> Reference }
  def reference    = rule { "(" ~ oneOrMore(ref ~ optional(";" ~ space)) ~ ")" }
  def quotation    = rule { ("\""     ~ words ~> identity ~ "\"" |
                             "''"     ~ words ~> identity ~ "''" |
                             "\u201C" ~ words ~> identity ~ "\u201D") ~~> Quotation }

  def fullRef      = rule { optional(quotation) ~ space ~ reference ~~> FullReference }
  def fullRefText  = rule { fullRef ~~> ((r: FullReference) => Results.fromFullReference(r)) }

  def normalText   = rule { !fullRefText ~ words ~> ((s: String) => Results(Words.fromString(s))) }
  def text         = rule { oneOrMore(fullRefText | normalText) ~~> ((results: List[Results]) => results.foldLeft(Results()) { _ add _ }) }

  def parse(input: String) = ReportingParseRunner(text).run(input).result.getOrElse(Results(message="incorrect input"))
  override val buildParseTree = true

}
