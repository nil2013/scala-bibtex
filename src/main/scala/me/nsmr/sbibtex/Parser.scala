package me.nsmr.sbibtex

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import java.io.InputStreamReader

class Parser(tokenizer: Tokenizer) {

  import Parser.{Reading, blockOperators}

  lazy val (next, remainder): (ParseResult[BibEntry], Parser) = {
    if (isEmpty) throw new IndexOutOfBoundsException("No more parsable tokens remained...")
    else {
      lazy val valuePattern = "[{\"](.*)[}\"]".r
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
          case reading@Reading.Result((_, "}")) => {
            stack match {
              case v :: "=" :: k :: Nil =>
                reading.map { case (e, _) => e.copy(values = e.values + (k -> v)) }
              case tag :: Nil =>
                reading.map { case (e, _) => e.copy(tags = e.tags + tag) }
              case _ =>
                reading.map(_._1)
            }
          }
          case reading@Reading.Result((_, tok)) =>
            blockOperators.find(_._1 == tok) match {
              case Some((_, terminator)) => blockParser(reading.map(_._1), terminator :: Nil, tok :: Nil) match {
                case (reading, valuePattern(token)) => bodyParser(reading, token :: stack)
              }
              case None => bodyParser(reading.map {
                _._1
              }, tok :: stack)
            }
        }
      }


      val reading = bodyParser(Reading(tokenizer).skipUntil(_ == "@").skipSpace().read().map { name =>
        BibEntry(new BibEntry.EntryType(name), Map.empty, Set.empty)
      }.skipUntil(_ == "{"), Nil)

      (ParseResult.Success(reading.result, reading.tokenizer.stack.reverse.mkString), new Parser(reading.tokenizer.discharge()))
    }
  }

  // TODO: check whether the condition is correct or not.
  def isEmpty: Boolean = tokenizer.isEmpty
}

object Parser {

  private lazy val blockOperators = List(("{", "}"), ("\"", "\""))

  def fromString(str: String): Parser = new Parser(Tokenizer.fromString(str))

  @deprecated
  def fromInputStreamReader(isr: InputStreamReader): Parser = new Parser(Tokenizer.fromInputStreamReader(isr))

  def readAll(str: String): List[ParseResult[BibEntry]] = {
    @tailrec def loop(parser: Parser, stack: List[ParseResult[BibEntry]]): List[ParseResult[BibEntry]] =
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

    loop(Parser.fromString(str), Nil)
  }

  private object Reading {

    class EntryPoint(reading: Reading[Unit]) {

      def read(): Reading[String] = this.reading.read().map(_._2)

      def readWhile(f: String => Boolean): Reading[List[String]] = this.reading.readWhile(f).map(_._2)

      def readUntil(f: String => Boolean): Reading[List[String]] = this.reading.readUntil(f).map(_._2)

      def readIf(f: String => Boolean): Reading[Option[String]] = this.reading.readIf(f).map(_._2)

      def skipWhile(f: String => Boolean): EntryPoint = new EntryPoint(this.reading.skipWhile(f))

      def skipUntil(f: String => Boolean): EntryPoint = new EntryPoint(this.reading.skipUntil(f))

      def skipSpace(): EntryPoint = new EntryPoint(this.reading.skipSpace())
    }

    def apply(tokenizer: Tokenizer): EntryPoint = new EntryPoint(new Reading(tokenizer, ()))

    object Result {
      def unapply[T](reading: Reading[T]): Option[T] = Option(reading.result)
    }

  }

  private class Reading[T](val tokenizer: Tokenizer, val result: T) {

    override def toString(): String = s"Parser.Reading(tokenizer = ${tokenizer}, result = ${result})"

    type Next = (T, List[String])

    def map[U](interpreter: T => U): Reading[U] = new Reading(tokenizer, interpreter(result))

    def read(): Reading[(T, String)] = {
      val next = tokenizer.read()
      new Reading(next, (result, next.stack.head))
    }

    def readWhile(f: String => Boolean): Reading[Next] = {
      @tailrec def loop(reading: Reading[T], stack: List[String]): Reading[Next] = {
        val next = reading.read()
        next.result match {
          case (_, token) if f(token) => loop(next.map(_._1), token :: stack)
          case _ => reading.map(result => (result, stack.reverse))
        }
      }
      loop(this, Nil)
    }

    def readUntil(f: String => Boolean): Reading[Next] ={
      @tailrec def loop(reading: Reading[T], stack: List[String]): Reading[Next] = {
        val next = reading.read()
        next.result match {
          case (result, token) if f(token) => next.map { _ => (result, (token :: stack).reverse) }
          case (_, token) => loop(next.map(_._1), token :: stack)
        }
      }
      loop(this, Nil)
    }

    def readIf(f: String => Boolean): Reading[(T, Option[String])] = {
      val next = this.read()
      if(f(next.result._2)) next.map { case (obj, token) => (obj, Option(token)) }
      else this.map { obj => (obj, None) }
    }

    def skipWhile(f: String => Boolean): Reading[T] =
      new Reading(this.tokenizer.skipWhile(f), this.result)

    def skipUntil(f: String => Boolean): Reading[T] =
      new Reading(this.tokenizer.skipUntil(f), this.result)

    def skipSpace(): Reading[T] =
      new Reading(this.tokenizer.skipSpace(), this.result)
  }

}
