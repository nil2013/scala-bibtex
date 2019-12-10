package me.nsmr.sbibtex

sealed abstract class ParseResult[T] {
  def source: String

  def result: T
}

object ParseResult {

  case class Success[T](result: T, source: String) extends ParseResult[T]

  case class Failure(source: String) extends ParseResult[Nothing] {
    override def result = throw new UnsupportedOperationException("Failure does not have any result...")
  }

}

