package essays
import docs.Doc._

case class Results(words: Words = Words(), references: Seq[FullReference] = Seq(), message: String = "", fullTextWordsNumber: Int = 0) {
  def add(other: Results) =
    Results(words.add(other.words),
            references ++ other.references,
            message+(if (!(message+other.message).isEmpty) ";" else "")+other.message,
            fullTextWordsNumber + other.fullTextWordsNumber)

  def wordsNumber = words.number
  def refsWordsNumber = references.foldLeft(0) { (res, cur) => res + cur.wordsNumber }
  def diffsNumber = fullTextWordsNumber - (wordsNumber + refsWordsNumber)
  
  def addQuote(quote: Option[String]) = this
  override def toString = "Results("+words+","+references+(if (message.isEmpty) "" else ","+message)+")"
}

object Results {
  def fromQuote(quote: Option[String]) = Results(references = Seq())
  def fromFullReference(ref: FullReference) = Results(references = Seq(ref))
}

case class Words(number: Int = 0) {
  def add(other: Words) = Words(number + other.number)
}
object Words {
  def fromString(s: String) = Words(s.split("\\s").size)
}

case class Quotation(quotation: String = "") {
  def wordsNumber = 0
}

case class Reference(ref: String="", years: Years = Years(), pages: Option[Pages] = None) {
  override def toString = (Seq[Any](ref, years) ++ pages.toSeq).mkString(", ")
  def wordsNumber = Seq(ref, years.toString, pages.toString).foldLeft(0) { _ + _.wordsNumber }
}
case class FullReference(quotation: Option[Quotation] = None, references: Seq[Reference] = Seq()) {
  def wordsNumber = 0
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
