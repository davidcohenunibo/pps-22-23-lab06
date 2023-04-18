package u06lab.code

/** Consider the Parser example shown in previous lesson. Analogously to NonEmpty, create a mixin NotTwoConsecutive,
  * which adds the idea that one cannot parse two consecutive elements which are equal. Use it (as a mixin) to build
  * class NotTwoConsecutiveParser, used in the testing code at the end. Note we also test that the two mixins can work
  * together!!


   Provide missing implementations such that the code in TryParsers works
  correctly.
  Consider the Parser example shown in previous lesson.
  Analogously to NonEmpty, create a mixin NotTwoConsecutive, which adds
  the idea that one cannot parse two consecutive elements which are equal.
  Use it (as a mixin) to build class NotTwoConsecutiveParser, used in the
  testing code at the end.
  Note we also test that the two mixins can work together!!
  Write the full linearisation of parserNTCNE
  N.B.: tests are written in such a way that each call to parseAll runs on a
  brand-new parser (got via a 0-arg def). If you want to avoid this (i.e.,
  running parseAll multiple times on the same parser object), you need to
  reset the parser after use (e.g., in parseAll)
  2) Extend Scala type String with a factory method that creates a parser which
  recognises the set of chars of a string.
  3) Optional Implement mixin ShortenThenN which accepts a sequence of chars
  of length at most n (part of the trait constructor)*/

abstract class Parser[T]:
  def parse(t: T): Boolean // is the token accepted?
  def end: Boolean // is it ok to end here
  def parseAll(seq: Seq[T]): Boolean = (seq forall parse) & end // note &, not &&

object Parsers:
  extension (s: String) def charParser(): Parser[Char] = new BasicParser(s.toSet)

class BasicParser(chars: Set[Char]) extends Parser[Char]:
  override def parse(t: Char): Boolean = chars.contains(t)
  override def end: Boolean = true

trait NonEmpty[T] extends Parser[T]:
    abstract override def parseAll(seq: Seq[T]): Boolean = seq.nonEmpty && super.parseAll(seq)

class NonEmptyParser(chars: Set[Char]) extends BasicParser(chars) with NonEmpty[Char]

trait NotTwoConsecutive[T] extends Parser[T]:
  abstract override def parseAll(seq: Seq[T]): Boolean = seq.sliding(2).forall {
    case Seq(x, y) => x != y && super.parseAll(seq)
    case _ => super.parseAll(seq)
  }

//Implment mixin ShortenThenN which accepts a sequence of chars
//of length at most n (part of the trait constructor).

trait ShortenThenN[T] extends Parser[T]:
  private[this] val n: Int = 5
  abstract override def parseAll(seq: Seq[T]): Boolean = seq.size <= n && super.parseAll(seq)

class NotTwoConsecutiveParser(chars: Set[Char]) extends BasicParser(chars) with NotTwoConsecutive[Char]

@main def checkParsers(): Unit =
  def parser = new BasicParser(Set('a', 'b', 'c'))
  println(parser.parseAll("aabc".toList)) // true
  println(parser.parseAll("aabcdc".toList)) // false
  println(parser.parseAll("".toList)) // true

  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))
  println(parserNE.parseAll("0101".toList)) // true
  println(parserNE.parseAll("0123".toList)) // false
  println(parserNE.parseAll(List())) // false

  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  println(parserNTC.parseAll("XYZ".toList)) // true
  println(parserNTC.parseAll("XYYZ".toList)) // false
  println(parserNTC.parseAll("".toList)) // true

  // note we do not need a class name here, we use the structural type
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char] with ShortenThenN[Char]
  println(parserNTCNE.parseAll("XYZ".toList)) // true
  println(parserNTCNE.parseAll("XYYZ".toList)) // false
  println(parserNTCNE.parseAll("".toList)) // false
//
//  import Parsers.*
//  def sparser: Parser[Char] = "abc".charParser()
//  println(sparser.parseAll("aabc".toList)) // true
//  println(sparser.parseAll("aabcdc".toList)) // false
//  println(sparser.parseAll("".toList)) // true
