package ex1

import org.scalatest.matchers.should.Matchers.*

class ParserTests extends org.scalatest.funsuite.AnyFunSuite:
  def parser = new BasicParser(Set('a', 'b', 'c'))
  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  // note we do not need a class name here, we use the structural type
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
  import Parsers.*
  def sparser: Parser[Char] = "abc".charParser()
  def parserSTN_T = new BasicParser(Set('X', 'Y', 'Z')) with ShortenThenN[Char](4)
  def parserSTN_F = new ShortenThenNParser(Set('X', 'Y', 'Z'))(2)

  test("testBasicParser"):
    parser.parseAll("aabc".toList) should be (true)
    parser.parseAll("aabcdc".toList) should be (false)
    parser.parseAll("".toList) should be (true)

  test("testNotEmptyParser"):
    parserNE.parseAll("0101".toList) should be (true)
    parserNE.parseAll("0123".toList) should be (false)
    parserNE.parseAll(List()) should be (false)

  test("testNotTwoConsecutiveParser"):
    parserNTC.parseAll("XYZ".toList) should be (true)
    parserNTC.parseAll("XYYZ".toList) should be (false)
    parserNTC.parseAll("".toList) should be (true)

  test("testNotEmptyAndNotTwoConsecutiveParser")
    parserNTCNE.parseAll("XYZ".toList) should be (true)
    parserNTCNE.parseAll("XYYZ".toList) should be (false)
    parserNTCNE.parseAll("".toList) should be (false)

  test("testStringParser"):
    sparser.parseAll("aabc".toList) should be (true)
    sparser.parseAll("aabcdc".toList) should be (false)
    sparser.parseAll("".toList) should be (true)

  test("testShortenThenNParser"):
    parserSTN_T.parseAll("XYZ".toList) should be (true)
    parserSTN_F.parseAll("XYZ".toList) should be (false)

