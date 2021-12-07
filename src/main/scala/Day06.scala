import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day06 extends App {

  type Population = Map[Int, Long] // size by timer

  implicit class RichPopulation(population: Population) {
    def afterOneDay(): Population = {
      population.toList.flatMap {
        case (k, v) if k == 0 => List((6, v), (8, v))
        case (k, v) => List((k - 1, v))
      }
        .groupMap(_._1)(_._2)
        .map { case (key, list) => (key, list.sum) }
    }

    @tailrec
    final def afterDays(days: Int): Population =
      if (days > 0)
        afterOneDay().afterDays(days - 1)
      else
        population

    def total(): Long = population.values.sum
  }

  val numbers = Using(Source.fromResource("day06.txt")) { source =>
    source.getLines().toList
  }.get.mkString(",")
    .split(",")
    .map(_.toInt)
    .toList

  val population = numbers
    .groupBy(identity)
    .map { case (num, list) => (num, list.length.toLong) }

  println(s"part 1: ${population.afterDays(80).total()}")
  println(s"part 2: ${population.afterDays(256).total()}")
}
