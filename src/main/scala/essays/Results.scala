package essays
import docs.Doc._

case class Results(words: Words = Words(), references: Seq[Reference] = Seq(), message: String = "", fullTextWordsNumber: Int = 0) {
  def add(other: Results) =
    Results(words.add(other.words),
            references ++ other.references,
            message+(if (!(message+other.message).isEmpty) ";" else "")+other.message,
            fullTextWordsNumber + other.fullTextWordsNumber)

  def wordsNumber = words.number
  def refsWordsNumber = references.foldLeft(0) { (res, cur) => res + cur.wordsNumber }
  def diffsNumber = fullTextWordsNumber - (wordsNumber + refsWordsNumber)
  
  def addQuote(quote: Option[String]) = copy(references = references.dropRight(1) ++ references.lastOption.map(r => r.addQuote(quote)).toSeq)
  override def toString = "Results("+words+","+references+(if (message.isEmpty) "" else ","+message)+")"
}

object Results {
  def fromQuote(quote: Option[String]) = Results(references = Reference.fromQuote(quote).toSeq)
}

case class Words(number: Int = 0) {
  def add(other: Words) = Words(number + other.number)
}
object Words {
  def fromString(s: String) = Words(s.split("\\s").size)
}

case class Reference(ref: String="", years: Years = Years(), pages: Option[Pages] = None, quote: String = "") {
  override def toString = (Seq[Any](ref, years) ++ pages.toSeq).mkString(", ")

  def addQuote(quote: Option[String]) = quote.map(q => copy(quote = q)).getOrElse(this)
  def wordsNumber = Seq(ref, years.toString, pages.toString, quote).foldLeft(0) { _ + _.wordsNumber }
}

object Reference {
  def apply(ref: String, years: Years, pages: Pages): Reference = new Reference(ref, years, Some(pages))
  def fromQuote(quote: Option[String]) = quote.map(q => Reference(quote = q))
}
case class Year(year: Int = 0, firstEdition: Boolean = false) {
  override def toString = if (firstEdition) "["+year.toString+"]" else year.toString
}
case class Years(year1: Year = Year(), separator: Option[String] = None, year2: Option[Year] = None) {
  override def toString = (Seq(year1) ++ year2.toSeq).mkString(separator.getOrElse("-"))
}
case class Pages(pages: String = "") {
  override def toString = pages
}

