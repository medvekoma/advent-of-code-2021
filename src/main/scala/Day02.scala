import scala.io.Source
import scala.util.Using

object Day02 extends App {

  val directions = Using(Source.fromResource("day02.txt")) { source =>
    source.getLines()
      .map(line => line.split(' '))
      .collect { case Array(direction, value) => (direction, value.toInt) }
      .toList
  }.get

  def solution1(directions: List[(String, Int)]): Int = {
    val result = directions.foldLeft((0, 0)) {
      case ((horz, vert), (command, value)) =>
        command match {
          case "forward" => (horz + value, vert)
          case "down" => (horz, vert + value)
          case "up" => (horz, vert - value)
        }
    }
    result._1 * result._2
  }

  println(s"part #1: ${solution1(directions)}")

  def solution2(directions: List[(String, Int)]): Int = {
    val result = directions.foldLeft((0, 0, 0)) {
      case ((horz, vert, aim), (command, value)) =>
        command match {
          case "forward" => (horz + value, vert + aim * value, aim)
          case "down" => (horz, vert, aim + value)
          case "up" => (horz, vert, aim - value)
        }
    }
    result._1 * result._2
  }

  println(s"part #2: ${solution2(directions)}")
}
