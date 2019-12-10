package me.nsmr.sbibtex

import java.io.InputStreamReader

import scala.annotation.tailrec

abstract class BibReader {
  def next: Char

  def nextSome(size: Int): (List[Char], BibReader) = {
    @tailrec def loop(cnt: Int, stack: List[Char], reader: BibReader): (List[Char], BibReader) = {
      if (cnt > 0) loop(cnt - 1, reader.next :: stack, reader.remainder)
      else {
        (stack.reverse, reader)
      }
    }

    loop(size, Nil, this)
  }

  def remainder: BibReader

  def isEmpty: Boolean
}

object BibReader {

  private case class DefaultReader(chars: List[Char]) extends BibReader {
    override def next: Char = chars.headOption.getOrElse(throw new IndexOutOfBoundsException("No more characters..."))

    override def remainder: DefaultReader = chars match {
      case Nil => throw new IndexOutOfBoundsException("No more remainder...")
      case head :: tail => DefaultReader(tail)
    }

    override def isEmpty = chars.isEmpty
  }

  @deprecated
  private case class InputStreamBibReader(is: InputStreamReader) extends BibReader {

    private[this] lazy val read: Int = is.read()

    override def next: Char =
      if (isEmpty) throw new IndexOutOfBoundsException("No more characters...")
      else read.toChar

    override def remainder: InputStreamBibReader =
      if (isEmpty) throw new IndexOutOfBoundsException("No more remainder...")
      else InputStreamBibReader(is)

    override def isEmpty = (read < 0)
  }

  def fromString(str: String): BibReader = DefaultReader(str.toList)

  @deprecated def fromInputStreamReader(is: InputStreamReader): BibReader = InputStreamBibReader(is)
}