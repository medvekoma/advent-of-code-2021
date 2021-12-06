import scala.io.Source
import scala.util.Using

object Day01 extends App {

  val numbers = Using(Source.fromResource("day01.txt")) { source =>
    source.getLines().map(_.toInt).toList
  }.get

  def measurementCount(numbers: Seq[Int]): Int = {
    val result = numbers.foldLeft((0, Int.MaxValue)) {
      case ((sum, prev), current) =>
        (if (current > prev) sum + 1 else sum, current)
    }
    result._1
  }

  println(s"part #1: ${measurementCount(numbers)}")

  val slidingNumbers = numbers
    .sliding(3, 1)
    .map(_.sum)
    .toSeq

  println(s"part #2: ${measurementCount(slidingNumbers)}")
}
