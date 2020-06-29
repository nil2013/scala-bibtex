package me.nsmr.sbibtex

sealed abstract class ParseResult[+T] {
  def source: String

  def result: T

  def isSuccess: Boolean

  def isFailure: Boolean = !this.isSuccess

  def toOption: Option[T] = if(this.isSuccess) Option(result) else None
}

object ParseResult {

  case class Success[+T](result: T, source: String) extends ParseResult[T] {

    override def isSuccess: Boolean = true

  }

  case class Failure(source: String) extends ParseResult[Nothing] {

    override def isSuccess: Boolean = false

    override def result = throw new UnsupportedOperationException("Failure does not have any result...")

   }

}

