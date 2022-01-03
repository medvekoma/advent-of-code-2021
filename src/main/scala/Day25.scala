import utils.ResourceFile

object Day25 extends App {

  val originalLines = ResourceFile.readLines("day25.txt")

  def stepLogic(cucumber: Char)(lines: List[String]): List[String] = {
    lines.map { line =>
      val text = (line.last + line + line.head).replace(s"$cucumber.", s".$cucumber")
      text.tail.take(line.length)
    }
  }

  def transpose(lines: List[String]): List[String] = lines.transpose.map(_.mkString)

  def stepEast = stepLogic('>') _

  def stepSouth = transpose _ andThen stepLogic('v') andThen transpose

  def step(lines: List[String]): Option[List[String]] = {
    val newLines = (stepEast andThen stepSouth) (lines)
    if (lines == newLines)
      None
    else
      Some(newLines)
  }

  var prevLines = originalLines
  var steps = 0
  var ready = false
  do {
    steps += 1
    step(prevLines) match {
      case None =>
        ready = true
      case Some(nextLines) =>
        prevLines = nextLines
    }
  } while (!ready)
  println(s"part 1: $steps")
}
