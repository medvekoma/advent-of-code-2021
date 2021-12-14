import scala.io.Source
import scala.util.Using

object Day12 extends App {

  val lines = Using(Source.fromResource("day12.txt")) {
    _.getLines().toList
  }.get

  val segmentMap = lines
    .map(_.split("-"))
    .collect { case Array(a, b) => Seq((a, b), (b, a)) }
    .flatten
    .filter { case (a, b) => b != "start" && a != "end" }
    .groupMap(_._1)(_._2)

  def discoverPaths(path: List[String], badPath: (List[String], String) => Boolean): List[List[String]] = {
    val last = path.last
    if (last == "end")
      List(path)
    else {
      segmentMap(path.last)
        .filterNot(next => badPath(path, next))
        .map(path :+ _)
        .flatMap(path => discoverPaths(path, badPath))
    }
  }

  def badPath1(path: List[String], next: String): Boolean =
    next.head.isLower && path.contains(next)

  def badPath2(path: List[String], next: String): Boolean =
    next.head.isLower &&
      (path :+ next)
        .filter(_.head.isLower)
        .groupBy(identity)
        .collect { case (cave, list) if list.size > 1 => list.size }
        .sum > 2

  val result1 = discoverPaths(List("start"), badPath1)
  println(result1.size)
  val result2 = discoverPaths(List("start"), badPath2)
  println(result2.size)
}
