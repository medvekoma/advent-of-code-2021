import scala.io.Source
import scala.util.Using

object Day14 extends App {

  val lines = Using(Source.fromResource("day14.txt")) {
    _.getLines().toList
  }.get

  val polymerString = lines.head

  // ABCAB: Map(AB -> 2, BC -> 1, CA -> 1)
  val polymer: Map[String, Long] = polymerString
    .sliding(2).toList
    .groupBy(identity)
    .map { case (elem, list) => (elem, list.size.toLong) }

  // AB -> X: Map(AB -> List(AX, XB))
  val rules: Map[String, List[String]] = lines
    .takeRight(lines.size - 2)
    .map(_.split(" -> "))
    .collect { case Array(a, b) => (a, a.head + b + a.last) }
    .map { case (a, b) => (a, b.sliding(2).toList) }
    .toMap

  implicit class Polymer(polymer: Map[String, Long]) {

    def expandOnce(): Map[String, Long] =
      polymer.toList
        .flatMap { case (pair, count) => rules.getOrElse(pair, List(pair)).map((_, count)) }
        .groupMap(_._1)(_._2)
        .map { case (key, values) => (key, values.sum) }

    def expand(times: Int): Map[String, Long] =
      (1 to times).foldLeft(polymer) { (current, _) =>
        current.expandOnce()
      }

    def evaluateWith(lastChar: Char): Long = {
      val charCounts = polymer.toList
        .map { case (pair, count) => (pair.head, count) } :+ (lastChar, 1L)

      val counts = charCounts
        .groupMap(_._1)(_._2)
        .map { case (char, count) => count.sum }
        .toList
        .sorted

      counts.last - counts.head
    }

    def length: Long =
      polymer.values.sum
  }

  val polymer10 = polymer.expand(times = 10)
  println(s"After 10 iterations: ${polymer10.evaluateWith(polymerString.last)}, length: ${polymer10.length}")

  val polymer40 = polymer.expand(times = 40)
  println(s"After 40 iterations: ${polymer40.evaluateWith(polymerString.last)}, length: ${polymer40.length}")
}
