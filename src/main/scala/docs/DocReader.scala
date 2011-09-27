package docs

import io.Source
import xml.XML
import scala.collection.JavaConversions._
import java.util.zip._

trait DocReader[T <: Doc] {
  def readFrom(t: T): Either[Exception, String]
}

object DocReader {
  implicit def reader(doc: Doc): DocumentReader = new DocumentReader(doc)
  class DocumentReader(doc: Doc) {
    def read =
      doc match {
        case d @ Docx(_)  => docxReader.readFrom(d)
        case d @ Odt(_)   => odtReader.readFrom(d)
        case d @ Txt(_)   => txtReader.readFrom(d)
        case d @ Other(_) => otherReader.readFrom(d)
      }
  }

  implicit def docxReader: DocReader[Docx] = new DocReader[Docx] {
    def readFrom(doc: Docx) = {
      val rootzip = new ZipFile(doc.path)
      rootzip.entries.find(_.getName.equals("word/document.xml")).
        map(f => XML.load(rootzip.getInputStream(f))) match {
          case Some(xml) => Right((xml \\ "p") map (p => (p \\ "t").map(_.text) mkString "") mkString "\n")
          case None      => Left(new Exception("The file "+doc.path+" could not be read"))
        }
      }
  }

  implicit def odtReader: DocReader[Odt] = new DocReader[Odt] {
    def readFrom(doc: Odt) = {
      val rootzip = new ZipFile(doc.path)
      rootzip.entries.find(_.getName.equals("content.xml")).map(f => XML.load(rootzip.getInputStream(f))) match {
        case Some(xml) => Right((xml \\ "p").map(_.text) mkString "\n")
        case None      => Left(new Exception("The file "+doc.path+" could not be read"))
      }
    }
  }
  implicit def txtReader: DocReader[Txt] = new DocReader[Txt] {
    def readFrom(doc: Txt) = {
      val source = Source.fromFile(doc.path, "ISO-8859-1")
      try {
        Right(source.mkString)
      } catch {
        case e: Exception => Left(e)
      } finally { source.close }
    }
  }
  implicit def otherReader: DocReader[Other] = new DocReader[Other] {
    def readFrom(doc: Other) = Left(new Exception("This format is not supported: "+doc.extension))
  }
}
