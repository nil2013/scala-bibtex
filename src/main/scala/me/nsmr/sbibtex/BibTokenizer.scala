package me.nsmr.sbibtex

import java.io.InputStreamReader

import scala.annotation.tailrec

class BibTokenizer(val stack: List[String], reader: BibReader) {

  import BibTokenizer._

  private lazy val nextTuple: (String, BibReader) = {
    @tailrec def loop(reader: BibReader, stack: List[Char]): (String, BibReader) = {
      if (reader.isEmpty) (stack.reverse.mkString, reader)
      else {
        reader.next match {
          case c if specialTokens.contains(c) =>
            if (stack.isEmpty) (c.toString, reader.remainder)
            else (stack.reverse.mkString, reader)
          case c => loop(reader.remainder, c :: stack)
        }
      }
    }

    if (isEmpty) throw new IndexOutOfBoundsException("No more tokens...")
    else {
      loop(reader, Nil)
    }
  }

  private def next: String = nextTuple._1

  private def remainder: BibReader = nextTuple._2

  def exhaustedTokens(): List[String] = this.stack.reverse

  def read(): BibTokenizer = new BibTokenizer(this.next :: this.stack, this.remainder)

  def discharge(): BibTokenizer = new BibTokenizer(Nil, this.reader)

  def map[T](f: (List[String], BibTokenizer) => T): T = f(this.stack, this)

  @deprecated
  def readSome(size: Int): BibTokenizer = {
    @tailrec def loop(cnt: Int, remainder: BibTokenizer): BibTokenizer = {
      if (cnt > 0) loop(cnt - 1, remainder.read())
      else remainder
    }

    loop(size, this)
  }

  def skipWhile(f: String => Boolean): BibTokenizer = {
    @tailrec def loop(remainder: BibTokenizer): BibTokenizer = {
      if (f(remainder.next)) loop(remainder.read())
      else remainder
    }

    loop(this)
  }

  def skipUntil(f: String => Boolean): BibTokenizer = {
    @tailrec def loop(remainder: BibTokenizer): BibTokenizer = {
      if (f(remainder.next)) remainder.read()
      else loop(remainder.read())
    }

    loop(this)
  }

  def skipSpace(): BibTokenizer = skipWhile(_.forall(_.isWhitespace))

//  def apply(): (List[String], BibTokenizer) = (this.stack, this)

  def isEmpty: Boolean = reader.isEmpty

//  private def push(token: String): BibTokenizer = new BibTokenizer(token :: this.stack, this.reader)

//  private def push(stack: List[String]): BibTokenizer = new BibTokenizer(stack ::: this.stack, this.reader)

}

object BibTokenizer {
  val specialTokens: Set[Char] = Set('@', '{', '}', '=', '"', ' ', '\n', ',')

  def fromString(str: String): BibTokenizer = new BibTokenizer(Nil, BibReader.fromString(str))

  @deprecated def fromInputStreamReader(is: InputStreamReader): BibTokenizer = new BibTokenizer(Nil, BibReader.fromInputStreamReader(is))

  def fromReader(reader: BibReader): BibTokenizer = new BibTokenizer(Nil, reader)
}