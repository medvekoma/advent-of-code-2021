import utils.ResourceFile

import scala.annotation.tailrec

object Day25 extends App {

  val originalLines = ResourceFile.readLines("day25.txt")

  val transpose = (lines: List[String]) => lines.transpose.map(_.mkString)

  val stepHorizontally = (sign: Char) => (lines: List[String]) =>
    lines.map { line =>
      (line.last + line + line.head)
        .replace(s"$sign.", s".$sign")
        .tail.take(line.length)
    }
  val stepEast = stepHorizontally('>')
  val stepSouth = transpose andThen stepHorizontally('v') andThen transpose
  val step = stepEast andThen stepSouth

  @tailrec
  def countSteps(prevLines: List[String], steps: Int = 0): Int =
    step(prevLines) match {
      case nextLines if nextLines != prevLines =>
        countSteps(nextLines, steps + 1)
      case _ =>
        steps + 1
    }

  println(s"part 1: ${countSteps(originalLines)}")
}
