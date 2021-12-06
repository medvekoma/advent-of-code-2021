import scala.io.Source
import scala.util.Using

object Day05 extends App {

  val lines = Using(Source.fromResource("day05.txt")) { source =>
    source.getLines().toList
  }.get

  val pattern = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r

  val sections = lines
    .map { case pattern(x1, y1, x2, y2) => ((x1.toInt, y1.toInt), (x2.toInt, y2.toInt))}
    .map (distribute)
    .groupMap(_._1)(_._2)

  val verticalPoints = sections.getOrElse("Vertical", List())
    .flatMap { case ((x1, y1), (x2, y2)) => (math.min(y1, y2) to math.max(y1, y2)).map(y => (x1, y))}
  val horizontalPoints = sections.getOrElse("Horizontal", List())
    .flatMap { case ((x1, y1), (x2, y2)) => (math.min(x1, x2) to math.max(x1, x2)).map(x => (x, y1))}
  val downRightPoints = sections.getOrElse("++", List())
    .flatMap { case ((x1, y1), (x2, y2)) => (x1 to x2).zip(y1 to y2) }
  val downLeftPoints = sections.getOrElse("+-", List())
    .flatMap { case ((x1, y1), (x2, y2)) => (x1 to x2).zip(y1 to y2 by -1) }

  val points1 = verticalPoints ++ horizontalPoints
  println(s"part 1: ${countMultiples(points1)}")

  val points2 = points1 ++ downRightPoints ++ downLeftPoints
  println(s"part 2: ${countMultiples(points2)}")

  def countMultiples(points: List[(Int, Int)]): Int =
    points
      .groupBy(identity)
      .count(_._2.length > 1)

  def distribute(input: ((Int, Int), (Int, Int))): (String, ((Int, Int), (Int, Int))) = {
    val ((x1, y1), (x2, y2)) = input
    if (x1 == x2)
      ("Vertical", ((x1, math.min(y1, y2)), (x1, math.max(y1, y2))))
    else if (y1 == y2)
      ("Horizontal", ((math.min(x1, x2), y1), (math.max(x1, x2), y1)))
    else if (x1 - x2 == y1 - y2)
      ("++", ((math.min(x1, x2), math.min(y1, y2)), (math.max(x1, x2), math.max(y1, y2))))
    else if (x1 - x2 == y2 - y1)
      ("+-", ((math.min(x1, x2), math.max(y1, y2)), (math.max(x1, x2), math.min(y1, y2))))
    else
      ("", ((0, 0), (0, 0)))
  }

}
