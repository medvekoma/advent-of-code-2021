import scala.io.Source
import scala.util.Using

object Day07 extends App {

  val positions = Using(Source.fromResource("day07.txt")) { source =>
    source.getLines()
      .buffered.head
      .split(",")
      .map(_.toInt)
      .toList
  }.get

  val groupedPositions = positions
    .groupBy(identity)
    .map { case (pos, list) => (pos, list.length) }

  def consumption1(a: Int, b: Int): Int =
    math.abs(a - b)

  def consumption2(a: Int, b: Int): Int = {
    val d = consumption1(a, b)
    d * (d + 1) / 2
  }

  def totalConsumptions(consumption: (Int, Int) => Int, groupedPositions: Map[Int, Int]): Seq[Int] = {
    (groupedPositions.keys.min to groupedPositions.keys.max)
      .map(position =>
        groupedPositions
          .map { case (pos, count) => consumption(pos, position) * count }
          .sum
      )
  }

  println(s"part 1: ${totalConsumptions(consumption1, groupedPositions).min}")
  println(s"part 2: ${totalConsumptions(consumption2, groupedPositions).min}")
}
