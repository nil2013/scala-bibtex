package me.nsmr.sbibtex


import scala.annotation.tailrec

object BibParser {
  val specialTokens = Set("@", "{", "}", "=", "\"", " ", "\n", ",")

  def tokenize(source: String): List[String] = {
    @tailrec def read(remain: String, chars: List[Char], tokens: List[String]): List[String] = {
      if (remain.isEmpty) {
        if (chars.isEmpty) tokens.reverse
        else read(remain, Nil, (chars.reverse.mkString) :: tokens)
      } else {
        specialTokens.find(t => remain.startsWith(t)) match {
          case Some(t) =>
            chars match {
              case Nil => read(remain.drop(t.length), Nil, t :: tokens)
              case chars => read(remain.drop(t.length), Nil, t :: (chars.reverse.mkString) :: tokens)
            }
          case None => read(remain.tail, remain.head :: chars, tokens)
        }
      }
    }

    read(source, Nil, Nil)
  }

  case class Tree(nodes: List[Node]) {
  }

  sealed abstract class Node {

    def isLeaf: Boolean = this.isInstanceOf[Leaf]

    def isLeafThen[A](f: Leaf => A): Option[A] = {
      if (this.isLeaf) Some(f(this.asInstanceOf[Leaf]))
      else None
    }

  }

  case class Leaf(body: String) extends Node {

    override def isLeaf = true

  }

  case class KeyValuePair(left: Leaf, right: Node) extends Node

  sealed abstract trait Block extends Node {
    def begin: String

    def body: List[Node]

    def end: String
  }

  case class Brace(override val body: List[Node]) extends Block {
    override def begin: String = "{"

    override def end: String = "}"
  }

  case class Quoted(override val body: List[Node]) extends Block {
    def begin: String = "\""

    def end: String = "\""
  }

  /**
   * ラフな構文木を構成します。
   *
   * @param tokens
   * @return
   */
  def buildTree(tokens: List[String]): Tree = {
    @tailrec def mainLoop(remain: List[String], stack: List[Node]): Tree = {
      remain match {
        case Nil => Tree(stack.reverse)
        case head :: tail if head.forall(_.isWhitespace) => mainLoop(tail, stack)
        case _ =>
          read(remain) match {
            case (remain, Brace(body)) =>
              mainLoop(remain, Brace(parseBody(body)) :: stack)
            case (remain, node) => mainLoop(remain, node :: stack)
          }
      }
    }

    def parseBody(nodes: List[Node]): List[Node] = {
      @tailrec def loop(remain: List[Node], stack: List[Node]): List[Node] = {
        remain match {
          case Nil => stack.reverse
          case Leaf(body) :: tail if body.forall(_.isWhitespace) => loop(tail, stack)
          case Leaf(",") :: tail => loop(tail, stack)
          case (head @ Leaf("=")) :: tail =>
            if (!stack.head.isLeaf) throw new UnexpectedNodeTypeException(stack.head)
            else loop(tail, head :: stack)
          case head :: tail =>
            stack match {
              case Leaf("=") :: key :: stack => loop(tail, KeyValuePair(key.asInstanceOf[Leaf], head) :: stack)
              case _ => loop(tail, head :: stack)
            }
        }
      }

      loop(nodes, Nil)
    }

    def read(remain: List[String]): (List[String], Node) = {
      remain match {
        case Nil => throw new BibSyntaxException("Unexpected EOL occurred")
        case "{" :: tail =>
          val (remain, tokens) = readBlock(tail, "}", Nil)
          (remain, Brace(tokens.reverse))
        case "\"" :: tail =>
          val (remain, tokens) = readBlock(tail, "\"", Nil)
          (remain, Quoted(tokens.reverse))
        case head :: tail => (tail, Leaf(head))
      }
    }

    @tailrec def readBlock(remain: List[String], until: String, stack: List[Node]): (List[String], List[Node]) = {
      remain match {
        case Nil =>
          if (until.isEmpty) (Nil, stack)
          else throw new BibSyntaxException(s"Unmatched Bracket: '${until}' not found...")
        case head :: tail if head == until => (tail, stack)
        case _ => {
          val (tail, node) = read(remain)
          readBlock(tail, until, node :: stack)
        }
      }
    }

    mainLoop(tokens, Nil)
  }

  def printIndented(node: Node): String = {
    val nl = System.lineSeparator()
    val sb = new StringBuilder
    node match {
      case Leaf(body) => sb.append(body)
      case Quoted(body) =>
        sb.append("\"").append(body.map(printIndented).mkString).append("\"")
      case Brace(body) =>
        sb.append("{").append(nl)
        sb.append(body.map(printIndented).mkString(nl).linesIterator.map(l => s"  $l").mkString(nl)).append(nl)
        sb.append("}")
      case KeyValuePair(left, right) =>
        sb.append(printIndented(left)).append("=").append(printIndented(right))
    }
    sb.toString()
  }

  def conjunction(nodes: List[Node], sep: String = ""): String = {
    def asSource(head: Node): String = head match {
      case Leaf(body) => body
      case KeyValuePair(Leaf(key), right) => new StringBuilder(key).append(asSource(right)).toString
      case Brace(nodes) => s"{${conjunction(nodes)}}"
      case Quoted(nodes) => s""""${conjunction(nodes)}""""
    }

    nodes.iterator.map(asSource).mkString
  }


  class BibSyntaxException(message: String) extends Exception(message)

  class UnexpectedNodeTypeException(node: Node) extends BibSyntaxException(s"Unexpected Node Type: '$node' is-a ${node.getClass.getSimpleName}")

  def load(source: String): BibParser = new BibParser(BibParser.buildTree(BibParser.tokenize(source)).nodes.iterator)
}

class BibParser(it: Iterator[BibParser.Node]) {

  import me.nsmr.sbibtex.BibParser.{Brace, KeyValuePair, Leaf, Node, Quoted, Tree, UnexpectedNodeTypeException, conjunction}

  def nextEntry: BibEntry = {
    it.dropWhile(_ != Leaf("@"))
    val entryType: String = it.dropWhile(_ != Leaf("@")).drop(1).next() match {
      case Leaf(label) => label
      case node => throw new UnexpectedNodeTypeException(node)
    }

    val (values, tags) = it.next match {
      case Brace(body) =>
        @tailrec def loop(remain: List[Node], values: List[KeyValuePair], tags: List[Leaf]): (Map[String, String], Set[String]) = {
          remain match {
            case Nil => (values.iterator.map { case KeyValuePair(left, right) =>
              left.body -> (right match {
                case Brace(body) => conjunction(body)
                case Quoted(body) => conjunction(body)
                case Leaf(body) => body
                case node => throw new UnexpectedNodeTypeException(node)
              })
            }.toMap, tags.iterator.map(_.body).toSet)
            case (head: KeyValuePair) :: tail => loop(tail, head :: values, tags)
            case (head: Leaf) :: tail => loop(tail, values, head :: tags)
            case head :: _ => throw new UnexpectedNodeTypeException(head)
          }
        }

        loop(body, Nil, Nil)
      case node => throw new UnexpectedNodeTypeException(node)
    }
    new BibEntry(new BibEntry.EntryType(entryType), values, tags)
  }

  def isFinished: Boolean = !it.hasNext
}
