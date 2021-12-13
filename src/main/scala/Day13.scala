import scala.io.Source
import scala.util.Using

object Day13 extends App {

  val lines = Using(Source.fromResource("day13.txt")) {
    _.getLines().toList
  }.get

  val coordinates = lines
    .takeWhile(_.nonEmpty)
    .map(_.split(","))
    .collect { case Array(x, y) => (x.toInt, y.toInt) }
    .toSet

  val instructions = lines
    .takeRight(lines.size - coordinates.size - 1)
    .map(_.split("="))
    .collect { case Array(inst, value) => (inst.last, value.toInt) }

  implicit class Sheet(coordinates: Set[(Int, Int)]) {
    val width: Int = coordinates.map(_._1).max + 1
    val height: Int = coordinates.map(_._2).max + 1

    def foldByX(foldAt: Int): Set[(Int, Int)] = {
      val page1 = coordinates.filter(_._1 < foldAt)
      val page2 = coordinates.filter(_._1 > foldAt)
        .map { case (x, y) => (x - foldAt - 1, y) }
      val page2width = page2.map(_._1).max + 1
      val mirroredPage2 = page2
        .map { case (x, y) => (page2width - x - 1, y) }
      page1 ++ mirroredPage2
    }

    def flip: Set[(Int, Int)] =
      coordinates.map { case (x, y) => (y, x) }

    def foldByY(foldAt: Int): Set[(Int, Int)] = {
      coordinates.flip.foldByX(foldAt).flip
    }

    def asCode: String = {
      val array = Array.tabulate(width, height) {
        (x, y) => if (coordinates.contains((x, y))) '#' else ' '
      }
      array.map(_.mkString).mkString("\n")
    }
  }

  val sheet1 = instructions.head match {
    case ('x', x) => coordinates.foldByX(x)
    case ('y', y) => coordinates.foldByY(y)
  }
  println(s"part 1: ${sheet1.size}")

  val foldedSheet = instructions.foldLeft(coordinates) { (sheet, instruction) =>
    instruction match {
      case ('x', foldX) => sheet.foldByX(foldX)
      case ('y', foldY) => sheet.foldByY(foldY)
    }
  }

  println("part 2:")
  println(foldedSheet.flip.asCode)
}
