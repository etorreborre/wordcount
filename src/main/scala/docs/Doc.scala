package docs

/**
 * Document type depending on the path extension
 */
sealed trait Doc {
  val path: String
  def extension = path.split("\\.").lastOption.getOrElse("")
}

case class Docx(path: String) extends Doc
case class Odt(path: String) extends Doc
case class Txt(path: String) extends Doc
case class Other(path: String) extends Doc

object Doc {
  implicit def pathToDoc(path: String): Doc = {
    if (path.endsWith(".docx"))     Docx(path)
    else if (path.endsWith(".odt")) Odt(path)
    else if (path.endsWith(".txt")) Txt(path)
    else                            Other(path)
  }

  implicit def toWordsNumber(t: String) = new {
    def wordsNumber = t.split("\\s").filter(_.nonEmpty).filterNot(_.trim matches "\\,|\\!|\\.|\\?|\\:|\\;").size
  }
}


