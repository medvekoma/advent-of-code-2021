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

  def discoverPart(path: List[String], condition: (String, List[String]) => Boolean): List[List[String]] = {
    val last = path.last
    if (last == "end")
      List(path)
    else {
      val nextElements = segmentMap(path.last)
      val (nextLarge, nextSmall) = nextElements.partition(_.head.isUpper)
      val pathsWithLargeLast = nextLarge.map(next => path :+ next)
      val pathsWithSmallLast = nextSmall.filter(cave => condition(cave, path)).map(path :+ _)
      (pathsWithLargeLast ++ pathsWithSmallLast)
        .flatMap(path => discoverPart(path, condition))
    }
  }

  def discoverPart1(path: List[String]): List[List[String]] = {
    val last = path.last
    if (last == "end")
      List(path)
    else {
      val nextElements = segmentMap(path.last)
      val (nextLarge, nextSmall) = nextElements.partition(_.head.isUpper)
      val pathsWithLargeLast = nextLarge.map(next => path :+ next)
      val pathsWithSmallLast = nextSmall.filter(cave => !path.contains(cave)).map(path :+ _)
      (pathsWithLargeLast ++ pathsWithSmallLast)
        .flatMap(path => discoverPart1(path))
    }
  }

  def discoverPart2(path: List[String]): List[List[String]] = {
    val last = path.last
    if (last == "end")
      List(path)
    else {
      val nextElements = segmentMap(path.last)
      val (nextLarge, nextSmall) = nextElements.partition(_.head.isUpper)
      val pathsWithLargeLast = nextLarge.map(next => path :+ next)
      val pathsWithSmallLast = nextSmall
        .filter { next =>
          val numberOfSmallCavesVisitedTwice = path.groupBy(identity)
            .count { case (cave, list) => cave.head.isLower && list.size == 2 }
          !path.contains(next) || numberOfSmallCavesVisitedTwice == 0
        }
        .map(path :+ _)
      (pathsWithLargeLast ++ pathsWithSmallLast)
        .flatMap(path => discoverPart2(path))
    }
  }

  val result = discoverPart1(List("start"))
  println(result.size)
  val result2 = discoverPart2(List("start"))
  println(result2.size)
}
