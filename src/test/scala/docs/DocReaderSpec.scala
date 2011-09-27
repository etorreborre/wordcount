package docs

import org.specs2._
import DocReader._

class DocReaderSpec extends Specification { def is  =

  "The text from an odt document can be extracted to a string"        ! e1^
  "The text from a docx document can be extracted to a string"        ! e2^
  "The text from an unknown document cannot be extracted to a string" ! e3^
                                                                      end

  def e1 = Odt("src/test/resources/hello-world.odt").read must beRight("Hello World\nHi")
  def e2 = Docx("src/test/resources/hello-world.docx").read must beRight("Hello World\nHi")
  def e3 = Other("src/test/resources/hello-world.xml").read must beLeft.like { case e => e.getMessage === "This format is not supported: xml" }
}