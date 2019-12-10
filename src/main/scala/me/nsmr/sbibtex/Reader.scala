package me.nsmr.sbibtex

import java.io.InputStreamReader

import scala.annotation.tailrec

abstract class Reader {
  def next: Char

  def nextSome(size: Int): (List[Char], Reader) = {
    @tailrec def loop(cnt: Int, stack: List[Char], reader: Reader): (List[Char], Reader) = {
      if (cnt > 0) loop(cnt - 1, reader.next :: stack, reader.remainder)
      else {
        (stack.reverse, reader)
      }
    }

    loop(size, Nil, this)
  }

  def remainder: Reader

  def isEmpty: Boolean
}

object Reader {

  private case class DefaultReader(chars: List[Char]) extends Reader {
    override def next: Char = chars.headOption.getOrElse(throw new IndexOutOfBoundsException("No more characters..."))

    override def remainder: DefaultReader = chars match {
      case Nil => throw new IndexOutOfBoundsException("No more remainder...")
      case head :: tail => DefaultReader(tail)
    }

    override def isEmpty = chars.isEmpty
  }

  @deprecated
  private case class ReaderUsingInputStreamReader(is: InputStreamReader) extends Reader {

    private[this] lazy val read: Int = is.read()

    override def next: Char =
      if (isEmpty) throw new IndexOutOfBoundsException("No more characters...")
      else read.toChar

    override def remainder: ReaderUsingInputStreamReader =
      if (isEmpty) throw new IndexOutOfBoundsException("No more remainder...")
      else ReaderUsingInputStreamReader(is)

    override def isEmpty = (read < 0)
  }

  def fromString(str: String): Reader = DefaultReader(str.toList)

  @deprecated
  def fromInputStreamReader(is: InputStreamReader): Reader = ReaderUsingInputStreamReader(is)
}