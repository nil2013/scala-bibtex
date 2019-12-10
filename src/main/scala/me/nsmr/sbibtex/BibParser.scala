package me.nsmr.sbibtex

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import java.io.InputStreamReader

class BibParser(tokenizer: BibTokenizer) {

  import BibParser.{Reading, blockOperators}

  lazy val (next, remainder): (ParseResult[BibEntry], BibParser) = {
    if (isEmpty) throw new IndexOutOfBoundsException("No more parsable tokens remained...")
    else {
      @tailrec def blockParser(reading: Reading[BibEntry], terminators: List[String], stack: List[String]): (Reading[BibEntry], String) = {
        terminators match {
          case Nil => (reading, stack.reverse.mkString)
          case terminator :: remainder =>
            reading.read() match {
              case reading@Reading.Result((_, tok)) =>
                blockParser(reading.map(_._1), {
                  if (tok == terminator) remainder
                  else blockOperators.find(_._1 == tok) match {
                    case Some((_, terminator)) => terminator :: terminators
                    case None => terminators
                  }
                }, tok :: stack)
            }
        }
      }

      @tailrec
      def bodyParser(reading: Reading[BibEntry], stack: List[String]): Reading[BibEntry] = {
        reading.skipSpace().read() match {
          case reading@Reading.Result((_, ",")) =>
            stack match {
              case v :: "=" :: k :: Nil =>
                bodyParser(reading.map { case (e, _) => e.copy(values = e.values + (k -> v)) }, Nil)
              case tag :: Nil => bodyParser(reading.map { case (e, _) => e.copy(tags = e.tags + tag) }, Nil)
              case _ => throw new IllegalArgumentException(s"Cannot interpret stack like: '${stack.reverse.mkString("', '")}'")
            }
          case reading@Reading.Result((_, "}")) => reading.map(_._1)
          case reading@Reading.Result((_, tok)) =>
            blockOperators.find(_._1 == tok) match {
              case Some((_, terminator)) => blockParser(reading.map(_._1), terminator :: Nil, tok :: Nil) match {
                case (reading, token) => bodyParser(reading, token :: stack)
              }
              case None => bodyParser(reading.map {
                _._1
              }, tok :: stack)
            }
        }
      }


      val reading = bodyParser(Reading(tokenizer).skipUntil(_ == "@").skipSpace().read().map { tokens =>
        BibEntry(new BibEntry.EntryType(tokens.mkString), Map.empty, Set.empty)
      }.skipUntil(_ == "{"), Nil)

      (ParseResult.Success(reading.result, reading.tokens.mkString), new BibParser(reading.tokenizer))
    }
  }

  // TODO: check whether the condition is correct or not.
  def isEmpty: Boolean = tokenizer.isEmpty
}

object BibParser {

  private lazy val blockOperators = List(("{", "}"), ("\"", "\""))

  def fromString(str: String): BibParser = new BibParser(BibTokenizer.fromString(str))

  @deprecated
  def fromInputStreamReader(isr: InputStreamReader): BibParser = new BibParser(BibTokenizer.fromInputStreamReader(isr))

  def readAll(str: String): List[ParseResult[BibEntry]] = {
    @tailrec def loop(parser: BibParser, stack: List[ParseResult[BibEntry]]): List[ParseResult[BibEntry]] =
      if (parser.isEmpty) stack.reverse
      else {
        Try {
          (parser.next, parser.remainder)
        } match {
          case Success((next, remainder)) =>
            loop(parser.remainder, parser.next :: stack)
          case Failure(e) =>
            e.printStackTrace()
            stack.reverse
        }
      }

    loop(BibParser.fromString(str), Nil)
  }

  private object Reading {

    class EntryPoint(reading: Reading[Unit]) {

      def read(): Reading[String] = this.reading.read().map(_._2)

      def readWhile(f: String => Boolean): Reading[List[String]] = this.reading.readWhile(f).map(_._2)

      def readUntil(f: String => Boolean): Reading[List[String]] = this.reading.readUntil(f).map(_._2)

      def readIf(f: String => Boolean): Reading[List[String]] = this.reading.readIf(f).map(_._2)

      def skipWhile(f: String => Boolean): EntryPoint = new EntryPoint(this.reading.skipWhile(f))

      def skipUntil(f: String => Boolean): EntryPoint = new EntryPoint(this.reading.skipUntil(f))

      def skipSpace(): EntryPoint = new EntryPoint(this.reading.skipSpace())
    }

    def apply(tokenizer: BibTokenizer): EntryPoint = new EntryPoint(new Reading(tokenizer, Nil, Unit))

    object Result {
      def unapply[T](reading: Reading[T]): Option[T] = Option(reading.result)
    }

  }

  private class Reading[T](val tokenizer: BibTokenizer, protected val stack: List[String], val result: T) {

    type Next = (T, List[String])

    def tokens: List[String] = tokenizer.stack.reverse

    def map[U](interpreter: T => U): Reading[U] = new Reading(tokenizer, stack, interpreter(result))

    def read(): Reading[(T, String)] =
      tokenizer.read().map {
        case (token :: _, tokenizer) => new Reading(tokenizer.discharge(), token :: stack, (result, token))
        case (Nil, _) => throw new IllegalArgumentException("No more element...")
      }

    def readWhile(f: String => Boolean): Reading[Next] =
      tokenizer.readWhile(f).map { (stack, tokenizer) =>
        new Reading(tokenizer.discharge(), stack ::: this.stack, (result, stack.reverse))
      }

    def readUntil(f: String => Boolean): Reading[Next] =
      tokenizer.readUntil(f).map { (stack, tokenizer) =>
        new Reading(tokenizer.discharge(), stack ::: this.stack, (result, stack.reverse))
      }

    def readIf(f: String => Boolean): Reading[Next] = {
      tokenizer.read().map {
        case (token :: Nil, tokenizer) if f(token) => new Reading(tokenizer.discharge(), token :: this.stack, (result, token :: Nil))
        case (token :: Nil, _) => throw new IllegalArgumentException(s"Unexpected Token found...: ${token}")
        case result => throw new IllegalArgumentException(s"Unexpected result returned: ${(result)}")
      }
    }

    def skipWhile(f: String => Boolean): Reading[T] =
      tokenizer.readWhile(f).map {
        (stack, tokenizer) => new Reading(tokenizer.discharge(), stack ::: this.stack, result)
      }

    def skipUntil(f: String => Boolean): Reading[T] =
      tokenizer.readUntil(f).map {
        (stack, tokenizer) => new Reading(tokenizer.discharge(), stack ::: this.stack, result)
      }

    def skipSpace(): Reading[T] = this.skipWhile(_.forall(_.isWhitespace))
  }

}
