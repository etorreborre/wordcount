package essays

import org.parboiled.scala._

trait ParboiledTextParsing extends Parser {

  // simple parsers
  def space        = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  def digit        = rule { "0" - "9" }
  def digits       = rule { oneOrMore(digit) }
  def letter       = rule { "a" - "z" | "A" - "Z" }
  def punctuation  = rule { anyOf("!?&.,;:-") }
  def word         = rule { oneOrMore(letter | digit) }
  def words        = rule { oneOrMore(word ~ (space | punctuation)) }
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

  def normalText   = rule { !fullRef ~ ANY ~> ((s: String) => Results(Words.fromString(s))) }
  def text         = rule { oneOrMore(normalText | fullRefText) ~~> ((results: List[Results]) => results.foldLeft(Results()) { _ add _ }) }

}
