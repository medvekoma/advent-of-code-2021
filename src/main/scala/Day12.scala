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
    .filter {
      case (a, b) => b != "start" && a != "end"
    }
    .groupMap(_._1)(_._2)

  def discoverPaths(path: List[String], condition: (List[String], String) => Boolean): List[List[String]] = {
    val last = path.last
    if (last == "end")
      List(path)
    else {
      val nextElements = segmentMap(path.last)
      val (nextLarge, nextSmall) = nextElements.partition(_.head.isUpper)
      val pathsWithLargeLast = nextLarge.map(next => path :+ next)
      val pathsWithSmallLast = nextSmall
        .filter(cave => condition(path, cave))
        .map(path :+ _)
      (pathsWithLargeLast ++ pathsWithSmallLast)
        .flatMap(path => discoverPaths(path, condition))
    }
  }

  def discoverPaths1(path: List[String]): List[List[String]] = {
    def condition(path: List[String], cave: String): Boolean =
      !path.contains(cave)

    discoverPaths(path, condition)
  }

  def discoverPaths2(path: List[String]): List[List[String]] = {
    def condition(path: List[String], cave: String): Boolean = {
      val smallCavesVisitedTwice = path.groupBy(identity)
        .filter { case (cave, list) => cave.head.isLower && list.size == 2 }
      !path.contains(cave) || smallCavesVisitedTwice.isEmpty
    }

    discoverPaths(path, condition)
  }

  val result = discoverPaths1(List("start"))
  println(result.size)
  val result2 = discoverPaths2(List("start"))
  println(result2.size)
}
