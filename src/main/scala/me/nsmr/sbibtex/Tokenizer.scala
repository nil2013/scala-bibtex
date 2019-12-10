package me.nsmr.sbibtex

import java.io.InputStreamReader

import scala.annotation.tailrec

class Tokenizer(val stack: List[String], reader: Reader) {

  import Tokenizer._

  private lazy val nextTuple: (String, Reader) = {
    @tailrec def loop(reader: Reader, stack: List[Char]): (String, Reader) = {
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

  private def remainder: Reader = nextTuple._2

  def exhaustedTokens(): List[String] = this.stack.reverse

  def read(): Tokenizer = new Tokenizer(this.next :: this.stack, this.remainder)

  def discharge(): Tokenizer = new Tokenizer(Nil, this.reader)

  def map[T](f: (List[String], Tokenizer) => T): T = f(this.stack, this)

  @deprecated
  def readSome(size: Int): Tokenizer = {
    @tailrec def loop(cnt: Int, remainder: Tokenizer): Tokenizer = {
      if (cnt > 0) loop(cnt - 1, remainder.read())
      else remainder
    }

    loop(size, this)
  }

  def skipWhile(f: String => Boolean): Tokenizer = {
    @tailrec def loop(remainder: Tokenizer): Tokenizer = {
      if (f(remainder.next)) loop(remainder.read())
      else remainder
    }

    loop(this)
  }

  def skipUntil(f: String => Boolean): Tokenizer = {
    @tailrec def loop(remainder: Tokenizer): Tokenizer = {
      if (f(remainder.next)) remainder.read()
      else loop(remainder.read())
    }

    loop(this)
  }

  def skipSpace(): Tokenizer = skipWhile(_.forall(_.isWhitespace))

  def isEmpty: Boolean = reader.isEmpty
}

object Tokenizer {
  val specialTokens: Set[Char] = Set('@', '{', '}', '=', '"', ' ', '\n', ',')

  def fromString(str: String): Tokenizer = new Tokenizer(Nil, Reader.fromString(str))

  @deprecated def fromInputStreamReader(is: InputStreamReader): Tokenizer = new Tokenizer(Nil, Reader.fromInputStreamReader(is))

  def fromReader(reader: Reader): Tokenizer = new Tokenizer(Nil, reader)
}