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

  val positionMap = positions
    .groupBy(identity)
    .map { case (pos, list) => (pos, list.length) }

  def consumption1(a: Int, b: Int): Int =
    math.abs(a - b)

  def consumption2(a: Int, b: Int): Int = {
    val d = consumption1(a, b)
    d * (d + 1) / 2
  }

  def totalConsumption(consumption: (Int, Int) => Int)(positionMap: Map[Int, Int], position: Int): Int = {
    positionMap
      .map { case (pos, count) => consumption(pos, position) * count }
      .sum
  }

  def findMinimum(distance: (Int, Int) => Int)(positionMap: Map[Int, Int]): Int = {
    (positionMap.keys.min to positionMap.keys.max)
      .map(totalConsumption(distance)(positionMap, _))
      .min
  }

  println(s"part 1: ${findMinimum(consumption1)(positionMap)}")
  println(s"part 2: ${findMinimum(consumption2)(positionMap)}")
}
