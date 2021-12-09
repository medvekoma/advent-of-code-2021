import scala.io.Source
import scala.util.Using

object Day09 extends App {

  val input = Using(Source.fromResource("day09.txt")) { source =>
    source.getLines()
      .map(_.toCharArray.map(_.asDigit).toList)
      .toList
  }.get

  class Board(board: List[List[Int]]) {

    val rows: Int = input.size
    val cols: Int = input.headOption.getOrElse(List.empty).size

    def neighbours(row: Int, col: Int): Seq[(Int, Int)] =
      Seq(
        (row - 1, col),
        (row + 1, col),
        (row, col - 1),
        (row, col + 1)
      ).filter {
        case (r, c) => (0 until rows).contains(r) && (0 until cols).contains(c)
      }

    def findLowPoints(): Seq[(Int, Int, Int)] =
      for (
        row <- board.indices;
        col <- board(row).indices;
        neighbourValues = neighbours(row, col).map { case (r, c) => board(r)(c) };
        value = board(row)(col)
        if neighbourValues.forall(_ > value)
      ) yield (value, row, col)

    def collectBasin(basin: Set[(Int, Int)], collected: Set[(Int, Int)] = Set.empty): Set[(Int, Int)] = {
      val allNeighbours = basin
        .flatMap { case (r, c) => neighbours(r, c) }
        .filter { case (r, c) => board(r)(c) < 9 }
      val newNeighbours = allNeighbours -- basin -- collected
      if (newNeighbours.isEmpty)
        basin
      else
        basin ++ collectBasin(newNeighbours, basin)
    }
  }

  val board = new Board(input)

  val lowPoints = board.findLowPoints()
  val lowValues = lowPoints.map(_._1)
  println(s"part 1: ${lowValues.sum + lowValues.size}")

  val largestBasins = lowPoints
    .map { case (_, r, c) => board.collectBasin(Set((r, c))).size }
    .sortBy(identity)(Ordering.Int.reverse)
    .take(3)
  println(s"part 2: ${largestBasins.product}")
}
