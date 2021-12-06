import scala.io.Source
import scala.util.Using

object Day06 extends App {

  val numbers = Using(Source.fromResource("day06.txt")) { source =>
    source.getLines().toList
  }.get.mkString(",")
    .split(",")
    .map(_.toInt)
    .toList

  def population(numbers: List[Int], steps: Int): Long = {
    val map2 = numbers
      .groupBy(identity)
      .map { case (num, list) => (num, list.length.toLong) }

    (1 to steps)
      .foldLeft(map2) { case (prev, n) => evolveOnce(prev) }
      .values.sum
  }
  def evolveOnce(generation: Map[Int, Long]): Map[Int, Long] = {
    generation.toList.flatMap {
      case (k, v) if k == 0 => List((6, v), (8, v))
      case (k, v) => List((k - 1, v))
    }
      .groupBy(_._1)
      .map { case (key, list) => (key, list.map(_._2).sum)}
  }


  println(s"part 1: ${population(numbers, 80)}")
  println(s"part 2: ${population(numbers, 256)}")

}
