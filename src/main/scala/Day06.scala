import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day06 extends App {

  val numbers = Using(Source.fromResource("day06.txt")) { source =>
    source.getLines().toList
  }.get.mkString(",")
    .split(",")
    .map(_.toInt)
    .toList

  val population = numbers
    .groupBy(identity)
    .map { case (num, list) => (num, list.length.toLong) }

  println(s"part 1: ${population.evolve(80).total()}")
  println(s"part 2: ${population.evolve(256).total()}")

  type Population = Map[Int, Long]

  implicit class RichPopulation(population: Population) {

    def evolveOnce(): Population = {
      population.toList.flatMap {
        case (k, v) if k == 0 => List((6, v), (8, v))
        case (k, v) => List((k - 1, v))
      }
        .groupBy(_._1)
        .map { case (key, list) => (key, list.map(_._2).sum) }
    }

    @tailrec
    final def evolve(times: Int): Population =
      if (times > 0)
        evolveOnce().evolve(times - 1)
      else
        population

    def total(): Long = population.values.sum
  }

}
