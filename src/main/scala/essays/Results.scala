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
  
  def addQuotation(quotation: Option[String]) = copy(references = references.dropRight(1) ++ references.lastOption.map(r => r.addQuotation(quotation)).toSeq)
  override def toString = "Results("+words+","+references+(if (message.isEmpty) "" else ","+message)+")"
}

object Results {
  def fromQuotation(quotation: Option[String]) = Results(references = Reference.fromQuotation(quotation).toSeq)
}

case class Words(number: Int = 0) {
  def add(other: Words) = Words(number + other.number)
}
object Words {
  def fromString(s: String) = Words(s.split("\\s").size)
}

case class Reference(ref: String = "", years: Years = Years(), pages: Option[Pages] = None, quotation: String = "") {
  override def toString = if (ref.isEmpty) quotation else (Seq[Any](ref, years) ++ pages.toSeq).mkString(", ")

  def addQuotation(quotation: Option[String]) = quotation.map(q => copy(quotation = q)).getOrElse(this)
  def wordsNumber =
    if (ref.isEmpty) quotation.wordsNumber else (Seq(ref, years, quotation) ++ pages.toSeq).foldLeft(0) { _ + _.toString.wordsNumber }
}

object Reference {
  def apply(ref: String, years: Years, pages: Pages): Reference = new Reference(ref, years, Some(pages))
  def fromQuotation(quotation: Option[String]) = quotation.map(q => Reference(quotation = q))
}
case class Year(year: Int = 0, firstEdition: Boolean = false) {
  override def toString = if (year == 0) "" else if (firstEdition) "["+year.toString+"]" else year.toString
}
case class Years(year1: Year = Year(), year2: Option[Year] = None, separator: String = "-") {
  override def toString = (Seq(year1) ++ year2.toSeq).mkString(separator)
}
case class Pages(pages: String = "") {
  override def toString = pages
}

