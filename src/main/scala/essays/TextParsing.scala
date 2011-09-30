package essays

import util.parsing._
import input._
import combinator.RegexParsers

/**
 * Parsers combinators for references in some text
 */
trait TextParsing extends RegexParsers {
  override def skipWhitespace = false

  /**
   * References parsing
   */
  def semicolumnSep: Parser[(Results, Results) => Results] = "\\;\\s*".r ^^^ { (_: Results).add(_) }

  def firstEdition: Parser[Year] = "\\[".r ~> "\\d\\d\\d\\d".r <~ "\\]".r ^^ { y => Year(y.toInt, firstEdition = true) }
  def laterEdition: Parser[Year] = "\\d\\d\\d\\d".r ^^ { y => Year(y.toInt) }

  def year: Parser[Year]   = firstEdition | laterEdition
  def years: Parser[Years] = year ~ "\\-|/|\\s*".r ~ year ^^ { case y1 ~ s ~ y2 => Years(y1, Some(y2), s) } |
                             year ~ year                  ^^ { case y1 ~ y2 => Years(y1, Some(y2)) } |
                             year                         ^^ { case y1          => Years(y1) }


  def page: Parser[Pages] = "p\\.*".r ~ "\\d*".r ^^ { case a ~ b => Pages(a+b) }
  def pageRange: Parser[Pages] = "(pp|p)\\.*".r ~ "\\d*".r ~ "\\-|/".r ~ "\\d*".r ^^ { case a ~ b ~ c ~ d => Pages(a+b+c+d) }
  def pages: Parser[Pages] = pageRange | page

  def commaNumberIs(i: Int) = guard(Seq.fill(i+1)(noComma).mkString("\\,").r)
  def noComma = "[^\\,\\(\\)\\;\\[\\]]+"

  def noRef = "[^\\(\\)]+".r ^^ { r => Results(Words.fromString(r)) }
  def notaReference = failure("not a reference") ^^^ Results()

  def makeRefYearsPages: PartialFunction[List[String] ~ Years ~ Pages, Results] =
    { case r ~ y ~ p => Results(references=Seq(Reference(r.mkString(", "), y, p))) }

  def makeRefYears: PartialFunction[List[String] ~ Years, Results] =
    { case r ~ y     => Results(references=Seq(Reference(r.mkString(", "), y))) }

  def referenceN(n: Int) = (1 to 5).foldLeft(notaReference) { (parser, i) =>
    repN(i, noComma.r <~ commaSep) ~ (years <~ commaSep) ~ pages ^^ makeRefYearsPages |
    repN(i, noComma.r <~ commaSep) ~ years                       ^^ makeRefYears      |
    parser
  }

  def reference: Parser[Results] = referenceN(5)
  def quotationText = ("\"|\u201C".r ~> "[^\"\u201D]+".r <~ "\"|\u201D".r) | "''.+?''".r ^^ (_.replace("''", ""))
  def quotation: Parser[Results] = quotationText ^^ { r => Results(references=Seq(Reference(quotation = r))) }

  def references: Parser[Results] =
    opt(quotationText <~ space) ~ ("(" ~> chainl1(reference, semicolumnSep) <~ ")") ^^ {  case q ~ r => r addQuotation q }
  def noRefParenthesised: Parser[Results] = "(" ~> noRef <~ ")"

  /**
   * General text parsing
   */
  def punctuation: Parser[Results] = "\\,|\\!|\\.|\\?|\\:|\\;".r ^^^ Results(Words(1))
  def commaSep: Parser[String] = "\\,\\s*".r
  def space: Parser[Results] = "\\s+".r ^^^ Results()
  def words: Parser[Results]  = "[^\\(\\)\"\\s]+".r ^^^ Results(Words(1))

  /**
   * The main parser
   */
  def referencedText: Parser[Results] = rep((references | noRefParenthesised | quotation | words | space) <~ opt(punctuation)) ^^ reduceResults

  def parse(source: Reader[Char]): Results = {
    parseAll(referencedText, source) match {
      case Success(results, rest) => results
      case Failure(msg, rest)     => Results(message = msg)
    }
  }

  lazy val reduceResults: PartialFunction[List[Results], Results] = { case results => (Results() /: results)  (_ add _) }
  implicit def stringToReader(s: =>String): Reader[Char] = new CharSequenceReader(s)

}

object TextParsing extends TextParsing
