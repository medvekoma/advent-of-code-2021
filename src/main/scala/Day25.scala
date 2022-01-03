import utils.ResourceFile

import scala.annotation.tailrec

object Day25 extends App {

  val originalLines = ResourceFile.readLines("day25.txt")

  def step(cucumber: Char)(lines: List[String]): List[String] = {
    lines.map { line =>
      val text = (line.last + line + line.head).replace(s"$cucumber.", s".$cucumber")
      text.tail.take(line.length)
    }
  }

  def transpose(lines: List[String]): List[String] = lines.transpose.map(_.mkString)

  def stepEast = step('>') _

  def stepSouth = transpose _ andThen step('v') andThen transpose

  def fullStep = stepEast andThen stepSouth

  @tailrec
  def evolve(lines: List[String], steps: Int = 0): Int = {
    val evolvedLines = fullStep(lines)
    if (evolvedLines == lines)
      steps + 1
    else
      evolve(evolvedLines, steps + 1)
  }

  println(s"part 1: ${evolve(originalLines)}")
}
