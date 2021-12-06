import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03 extends App {

  type BinaryNumber = Seq[Int]

  implicit class RichBinaryNumber(digits: BinaryNumber) {
    def asDecimal(): Int = Integer.parseInt(digits.mkString, 2)
  }

  val input = Using(Source.fromResource("day03.txt")) { source =>
    source.getLines()
      .map(_.toCharArray.map(_.asDigit).toList)
      .toList
  }.get

  val measurements = input
    .transpose

  val gammaValues = measurements
    .map { line => (line.sum, line.size)}
    .map { case (sum, size) => if (sum > size / 2) 1 else 0 }

  val epsilonValues = gammaValues
    .map(c => if (c==0) 1 else 0)

  val gamma = gammaValues.asDecimal()
  val epsilon = epsilonValues.asDecimal()

  println(s"part 1: ${gamma * epsilon}")

  @tailrec
  def findMarker(comparer: (Int, Int) => Boolean)(input: List[BinaryNumber], pos: Int = 0): List[BinaryNumber] = {
    if (input.size <= 1)
      return input

    val map = input.groupBy { x => x(pos) }
    (map.get(0), map.get(1)) match {
      case (Some(zeros), Some(ones)) =>
        if (comparer(zeros.size, ones.size))
          findMarker(comparer)(zeros, pos+1)
        else
          findMarker(comparer)(ones, pos+1)
      case _ =>
        findMarker(comparer)(input, pos+1)
    }
  }

  val oxygen = findMarker(_ > _)(input).head.asDecimal()
  val co2 = findMarker(_ <= _)(input).head.asDecimal()
  println(s"part 2: ${oxygen * co2}")
}
