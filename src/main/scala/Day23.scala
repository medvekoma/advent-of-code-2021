import utils.{MutableMatrix, ResourceFile}

object Day23 extends App {

  val lines = ResourceFile.readLines("day23.txt")
  val width = lines.head.length

  val start = lines.tail.take(lines.size - 2)
    .map(line => line.replace('.', ' ').padTo(width, ' ').toCharArray)
    .toArray

  val board = new MutableMatrix(start)
  val restPlaces = Seq(1, 2, 4, 6, 8, 10, 11).map((0, _))
  val homeColumns = Map('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)

  def home(letter: Char): Option[(Int, Int)] = {
    val col = homeColumns(letter)
    val columnText = (1 until board.rows).map(row => board((row, col))).mkString
    val pattern = s"( +)$letter*".r
    columnText match {
      case pattern(spaces) => Some((spaces.length, col))
      case _ => None
    }
  }

  println(board)
}
