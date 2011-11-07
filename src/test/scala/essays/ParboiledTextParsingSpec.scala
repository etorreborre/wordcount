package essays

import org.specs2._
import execute._
import org.parboiled.scala._
import parserunners.ReportingParseRunner
import testing.ParboiledTest

class ParboiledTextParsingSpec extends Specification with ParboiledTextParsing with ParboiledTesting { def is  =

"Words"                                                                                                                                     ^
  { words.parse("hello - you") === "hello - you" }                                                                                          ^
  { words.parse("hello world") === "hello world" }                                                                                          ^
  { words.parse("[in brackets]") === "[in brackets]" }                                                                                      ^
                                                                                                                                            p^
"Pages"                                                                                                                                     ^
  { pages.parse("p.3") === Pages("p.3") }                                                                                                   ^
  { pages.parse("p.34") === Pages("p.34") }                                                                                                 ^
  { pages.parse("p34") === Pages("p34") }                                                                                                   ^
  { pages.parse("p 34") === Pages("p 34") }                                                                                                 ^
  { pages.parse("p.215/324") === Pages("p.215/324") }                                                                                       ^
  { pages.parse("pp.215-324") === Pages("pp.215-324") }                                                                                     ^
  { pages.parse("pp.215/324") === Pages("pp.215/324") }                                                                                     ^
                                                                                                                                            p^
"Years"                                                                                                                                     ^
  { years.parse("1905") === Years(Year(1905)) }                                                                                             ^
  { years.parse("1905-1910") === Years(Year(1905), Some("-"), Some(Year(1910))) }                                                           ^
  { years.parse("1905/1910") === Years(Year(1905), Some("/"), Some(Year(1910))) }                                                           ^
  { years.parse("[1905]-1910") === Years(Year(1905, firstEdition=true), Some("-"), Some(Year(1910))) }                                      ^
  { years.parse("[1905]1910") === Years(Year(1905, firstEdition=true), None, Some(Year(1910))) }                                      ^
  { years.parse("1905 1910") === Years(Year(1905), Some(" ")  , Some(Year(1910))) }                                                         ^
                                                                                                                                            p^
"Reference"                                                                                                                                 ^
  { reference.parse("(Freud, 1905)") === List(Reference("Freud", Years(Year(1905)), None)) }                         ^
  { reference.parse("(Freud, 1905, p.134)") === List(Reference("Freud", Years(Year(1905)), Some(Pages("p.134")))) }                         ^
  { reference.parse("(Freud, Lacan, 1905, p.134)") === List(Reference("Freud, Lacan", Years(Year(1905)), Some(Pages("p.134")))) }           ^
  { reference.parse("(Freud in St. Clair, 1905, p.134)") === List(Reference("Freud in St. Clair", Years(Year(1905)), Some(Pages("p.134")))) }
  { reference.parse("(Freud, 1905 cited in St. Clair, 1905, p.134)") ===
    List(Reference("Freud, 1905 cited in St. Clair", Years(Year(1905)), Some(Pages("p.134")))) }                                            ^
  { reference.parse("(Freud, 1905, p.134; Lacan, 1969, p.97)") ===
    List(Reference("Freud", Years(Year(1905)), Some(Pages("p.134"))),
         Reference("Lacan", Years(Year(1969)), Some(Pages("p.97")))) }                                                                      ^
                                                                                                                                            p^
"Quotation"                                                                                                                                 ^
  { quotation.parse("''this is a quotation''") === Quotation("this is a quotation") }                                                       ^
                                                                                                                                            p^
"FullReference"                                                                                                                             ^
  { fullRef.parse("''this is a quotation'' (Freud, 1905, p.134)") ===
    FullReference(Some(Quotation("this is a quotation")), List(Reference("Freud", Years(Year(1905)), Some(Pages("p.134"))))) }              ^
                                                                                                                                            p^
"Counting"                                                                                                                                  ^
  { parse("''this is a quotation'' (Freud, 1905, p.134)").wordsNumber === 0 }                                                               ^
  { parse("""say "this is a quotation" (Freud, 1905, p.134)""").wordsNumber === 1 }                                                         ^
  { parse("hello (Freud, 1905, p.134) Right").wordsNumber === 2 }                                                                           ^
                                                                                                                                            end

}

trait ParboiledTesting extends ParboiledTest { outer =>
  implicit def useRule1[T](rule: Rule1[T]): ReportingParserRunnerRule1[T] = new ReportingParserRunnerRule1[T](rule)

  class ReportingParserRunnerRule1[T](rule: Rule1[T]) {
    def parse(input: String) = outer.parse(rule, input)
  }
  implicit def useRule0(rule: Rule0): ReportingParserRunnerRule0 = new ReportingParserRunnerRule0(rule)

  class ReportingParserRunnerRule0(rule: Rule0) {
    def parse(input: String) = outer.parse(rule, input)
  }
  def parse[T](rule: Rule1[T], input: String) = {
    ReportingParseRunner(rule).run(input).result.getOrElse("wrong input\n"+parseTree)
  }

  type Result = String
  def parse(rule: Rule0, input: String) = {
    super.parse(ReportingParseRunner(rule), input) { }
    input
  }

  def fail(msg: String): Nothing = throw new FailureException(Failure("wrong input\n"+parseTree))
}

