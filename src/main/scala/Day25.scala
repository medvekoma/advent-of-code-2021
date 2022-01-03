import utils.ResourceFile

import scala.annotation.tailrec

object Day25 extends App {

  val originalLines = ResourceFile.readLines("day25.txt")

  val transpose = (lines: List[String]) => lines.transpose.map(_.mkString)

  def stepHorizontally(sign: Char)(lines: List[String]): List[String] =
    lines.map { line =>
      val text = (line.last + line + line.head).replace(s"$sign.", s".$sign")
      text.tail.take(line.length)
    }

  val stepEast = stepHorizontally('>') _
  val stepSouth = transpose andThen stepHorizontally('v') andThen transpose
  val step = stepEast andThen stepSouth

  @tailrec
  def countSteps(lines: List[String], steps: Int = 0): Int =
    step(lines) match {
      case evolvedLines if evolvedLines != lines =>
        countSteps(evolvedLines, steps + 1)
      case _ =>
        steps + 1
    }

  println(s"part 1: ${countSteps(originalLines)}")
}
