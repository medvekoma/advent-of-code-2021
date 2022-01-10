import utils.ResourceFile

import scala.annotation.tailrec
import scala.math.{ceil, floor}
import scala.util.matching.Regex

object Day18 extends App {

  val pairPattern: Regex = "\\[(\\d+),(\\d+)]".r
  val lastNumberPattern: Regex = "(\\d+)[^\\d]*$".r
  val firstNumberPattern: Regex = "(\\d+)".r
  val splitPattern: Regex = "\\d\\d+".r

  val lines = ResourceFile.readLines("day18.txt")

  var result = reduce(lines.head)
  for (line <- lines.tail) {
    val addition = s"[$result,$line]"
    result = reduce(addition)
  }

  def explode(body: String): Option[String] = {
    val result: Option[Regex.Match] = pairPattern.findAllMatchIn(body)
      .find(m => depthOf(body, m.start) >= 4)
    result match {
      case None => None
      case Some(m) =>
        val a = m.group(1).toInt
        val b = m.group(2).toInt
        val left = body.take(m.start)
        val newLeft = lastNumberPattern.findFirstMatchIn(left)
          .map(m => left.take(m.start) + (m.group(1).toInt + a) + left.substring(m.start + m.group(1).length))
          .getOrElse(left)
        val right = body.substring(m.start + m.toString.length)
        val newRight = firstNumberPattern.findFirstMatchIn(right)
          .map(m => right.take(m.start) + (m.group(1).toInt + b) + right.substring(m.start + m.group(1).length))
          .getOrElse(right)
        Some(s"${newLeft}0$newRight")
    }
  }

  def split(body: String): Option[String] = {
    splitPattern.findFirstMatchIn(body) match {
      case Some(m) =>
        val half = m.toString.toInt / 2f
        val pair = s"[${floor(half).toInt},${ceil(half).toInt}]"
        Some(body.take(m.start) + pair + body.substring(m.end))
      case None =>
        None
    }
  }

  def reduceOnce(body: String): Option[String] =
    explode(body).orElse(split(body))

  @tailrec
  def reduce(body: String): String = {
    reduceOnce(body) match {
      case None => body
      case Some(text) =>
        reduce(text)
    }
  }

  def depthOf(body: String, pos: Int): Int = {
    val text = body.take(pos)
    text.count(_ == '[') - text.count(_ == ']')
  }

  sealed trait Node {
    def magnitude: Long
  }

  case class Number(n: Int) extends Node {
    override def magnitude: Long = n
  }

  case class Pair(a: Node, b: Node) extends Node {
    override def magnitude: Long = 3 * a.magnitude + 2 * b.magnitude
  }

  class Parser(body: String) {
    private var head: Int = 0

    def parse(): Node =
      body(head) match {
        case '[' => parsePair()
        case n if ('0' to '9').contains(n) => parseNumber()
        case _ => throw new RuntimeException(s"Syntax error at: ${body.substring(head)}")
      }

    def parseNumber(): Node = {
      val str = body.substring(head).takeWhile(_.isDigit)
      head += str.length
      Number(str.toInt)
    }

    def read(chars: Int): String = {
      head += chars
      body.substring(head - chars, head)
    }

    def parsePair(): Node = {
      read(1) // [
      val a = parse()
      read(1) // ,
      val b = parse()
      read(1) // ]
      Pair(a, b)
    }
  }

  val node = new Parser(result).parse()
  println(s"part 1: ${node.magnitude}")

  val magnitudes = for (
    a <- lines;
    b <- lines if a != b;
    sum = reduce(s"[$a,$b]");
    mag = new Parser(sum).parse().magnitude
  ) yield mag
  println(s"part 2: ${magnitudes.max}")

}
