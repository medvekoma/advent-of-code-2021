import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day16 extends App {

  val lines = Using(Source.fromResource("day16.txt")) {
    _.getLines().toList
  }.get

  sealed trait Packet {
    def versionSum(): Int = {
      this match {
        case LiteralPacket(version, _) => version
        case OperatorPacket(version, _, children) => version + children.map(_.versionSum()).sum
      }
    }

    def evaluate(): Long =
      this match {
        case LiteralPacket(_, value) => value
        case OperatorPacket(_, typeId, children) => typeId match {
          case 0 => children.map(_.evaluate()).sum
          case 1 => children.map(_.evaluate()).product
          case 2 => children.map(_.evaluate()).min
          case 3 => children.map(_.evaluate()).max
          case 5 => if (children.head.evaluate() > children.last.evaluate()) 1L else 0L
          case 6 => if (children.head.evaluate() < children.last.evaluate()) 1L else 0L
          case 7 => if (children.head.evaluate() == children.last.evaluate()) 1L else 0L
        }
      }
  }

  case class LiteralPacket(version: Int, value: Long) extends Packet

  case class OperatorPacket(version: Int, typeId: Int, children: Seq[Packet]) extends Packet

  class Parser(hex: String) {

    // binary string to evaluate
    private val text: String = hex.sliding(4, 4)
      .map(hx => Integer.parseInt(hx, 16).toBinaryString.reverse.padTo(hx.length * 4, '0').reverse)
      .mkString

    // reading position
    private var head: Int = 0

    def read(chars: Int): String = {
      head += chars
      text.substring(head - chars, head)
    }

    def readInt(chars: Int): Int = Integer.parseInt(read(chars), 2)

    def parsePacket(): Packet = {
      val version = readInt(3)
      val typeId = readInt(3)
      typeId match {
        case 4 => LiteralPacket(version, parseLiteralValue())
        case _ => OperatorPacket(version, typeId, parseOperatorValues())
      }
    }

    def parseLiteralValue(): Long = {
      val blocks = parseLiteralBlocks()
      convertBinaryString(blocks.mkString)
    }

    @tailrec
    private def parseLiteralBlocks(initial: Seq[String] = Seq.empty): Seq[String] = {
      parseLiteralBlock() match {
        case ('0', chunk) => initial :+ chunk
        case ('1', chunk) => parseLiteralBlocks(initial :+ chunk)
      }
    }

    def parseLiteralBlock(): (Char, String) = {
      val chunk = read(5)
      (chunk.head, chunk.tail)
    }

    def convertBinaryString(binary: String): Long = {
      binary.reverse.foldLeft((0L, 1L)) {
        case ((total, pow), bit) => (total + bit.asDigit * pow, 2 * pow)
      }._1
    }

    def parseOperatorValues(): Seq[Packet] = {
      val lengthId = readInt(1)
      lengthId match {
        case 0 => parseOperatorValues0()
        case 1 => parseOperatorValues1()
      }
    }

    def parseOperatorValues0(): Seq[Packet] = {
      val length = readInt(15)
      parseOperatorValuesUntil(head + length)
    }

    @tailrec
    private def parseOperatorValuesUntil(headUntil: Int, result: Seq[Packet] = Seq.empty): Seq[Packet] = {
      if (head >= headUntil)
        result
      else
        parseOperatorValuesUntil(headUntil, result :+ parsePacket())
    }

    def parseOperatorValues1(): Seq[Packet] = {
      val length = readInt(11)
      (1 to length).map(_ => parsePacket())
    }
  }

  val packet = new Parser(lines.head).parsePacket()

  println(s"part 1: ${packet.versionSum()}")
  println(s"part 2: ${packet.evaluate()}")
}
