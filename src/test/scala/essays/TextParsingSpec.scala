package essays

import org.specs2._
import matcher._
import TextParsing._

class TextParsingSpec extends Specification with ParserMatchers with ResultMatchers { def is  =

"Parsers"                                                                                                                        ^
  "Normal text"                                                                                                                  ^
    "can have special characters"                                                                                                ! text.e1^
                                                                                                                                 p^
  "References"                                                                                                                   ^
  { reference must succeedOn("reference, 1984, p.88") }                                                                          ^
  { reference must succeedOn("reference in St. Clair, 1984, p.88") }                                                             ^
  { reference must succeedOn("reference in St. Clair, 1984") }                                                                   ^
  { reference must succeedOn("Freud, 1905; Main, 1996") }                                                                        ^
  { reference must succeedOn("Winicott 1951/1958, cited in St. Clair, 2004, p.72") }                                             ^
  { reference must succeedOn("Sollod, Wilson and Monte, 2009, p.143") }                                                          ^
  { references must succeedOn("(reference in St. Clair, 1984, p.88)") }                                                          ^
  { references must succeedOn("(Yalom, 2005, pp.31-32)") }                                                                       ^
  { references must succeedOn("(reference in St. Clair, 1984)") }                                                                ^
  { references must succeedOn("(Freud, 1905; Main, 1996)") }                                                                     ^
  { references must succeedOn(""""hi mate" (me, 2011)""") }                                                                      ^
  { references must succeedOn("(Winicott 1951/1958, cited in St. Clair, 2004, p.72)") }                                          ^
  { references must succeedOn("(Kierkegaard, 1957, cited in Yalom, 1980, p.43)") }                                               ^
  { references must succeedOn("(Kierkegaard, 1957, cited in Yalom, 1980)") }                                                     ^
  { references must succeedOn("(Sollod, Wilson and Monte, 2009, p.143)") }                                                       ^
  { references must succeedOn("(Freud, [1905]1962, pp.95-96)") }                                                                 ^
  { references must succeedOn("(Freud, [1905]1962, pp.95-96; Main, 1996, pp.239-240)") }                                         ^
                                                                                                                                 p^
  "Years"                                                                                                                        ^
  { year must succeedOn("1984") }                                                                                                ^
  { years must succeedOn("1984") }                                                                                               ^
  { years must succeedOn("1934-1958") }                                                                                          ^
  { years must succeedOn("1934/1954") }                                                                                          ^
  { years must succeedOn("[1905]1962") }                                                                                         ^
  { years must succeedOn("[1905] 1962") }                                                                                        ^
                                                                                                                                 p^
  "Pages"                                                                                                                        ^
  { page must succeedOn("p.33") }                                                                                                ^
  { page must succeedOn("p33") }                                                                                                 ^
  { pages must succeedOn("p33") }                                                                                                ^
  { pages must succeedOn("p.33") }                                                                                               ^
  { pages must succeedOn("p.215/324") }                                                                                          ^
  { pages must succeedOn("pp.215-324") }                                                                                         ^
  { pages must succeedOn("pp.215/324") }                                                                                         ^
  { pages("pp. 215/324") must haveSuccessResult(===(Pages("pp. 215/324"))) }                                                     ^
                                                                                                                                 p^
  "Words"                                                                                                                        ^
  { words must succeedOn("hello") }                                                                                              ^
  { words must succeedOn("hello - you") }                                                                                        ^
  { words must succeedOn("hello - you") }                                                                                        ^
  { words must succeedOn("hello world") }                                                                                        ^
  { words must succeedOn("[in brackets]") }                                                                                      ^
                                                                                                                                 p^
"Whole text parsing"                                                                                                             ^
  "all words must be counted in normal text"                                                                                     ! parsing.e1^
  "non-reference text in () must be counted"                                                                                     ! parsing.e2^
  "no words must be counted in references"                                                                                       ! parsing.e3^
  "single punctuation characters can follow a ref"                                                                               ! parsing.e4^
  "words must be counted ok when references and text are interleaved"                                                            ! parsing.e5^
  """a quotation is some text between "". It must not be counted"""                                                              ! parsing.e6^
  "a quotation can be followed by a reference. It must not be counted"                                                           ! parsing.e7^
                                                                                                                                 p^
  "additional examples"                                                                                                          ^
  { parse("hello (reference, 1984) world").words must_== Words(2) }                                                              ^
  { parse("hi \"hello world\" (reference, 1984, p.96) you").words must_== Words(2) }                                             ^
                                                                                                                                 end

  object text {
    def e1 = words must succeedOn(""",.;:-"'?!&/()#_""")
  }

  object parsing {
    def e1 = parse("hello world") must_== Results(Words(2))
    def e2 = parse("(hello world)") must_== Results(Words(2))
    def e3 = parse("(reference, 1984, p.88)") must_==
             Results(words = Words(), references = Seq(Reference("reference", Years(Year(1984)), Some(Pages("p.88")))))
    def e4 = parse("(reference, 1984, p.88)!").words must_== Words(0)
    def e5 = parse("hello (reference, 1984, p.88) world").words must_== Words(2)
    def e6 = parse("""Freud said: "Hello Martha" (My Life, 1923, p.38). Incredible!""").words must_== Words(3)
    def e7 = parse("""Hey ''you dont'' he told me""").words must_== Words(4)

  }

  val parsers = TextParsing
}
