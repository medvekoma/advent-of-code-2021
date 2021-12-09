import scala.io.Source
import scala.util.Using

object Day09 extends App {

  val input = Using(Source.fromResource("day09.txt")) { source =>
    source.getLines()
      .map(_.toCharArray.map(_.asDigit).toList)
      .toList
  }.get

  class Board(input: List[List[Int]]) {

    // create board surrounded with 9s to avoid testing for out of bounds
    private val board = {
      val cols = input.headOption.getOrElse(List()).size
      val nineRow = List.fill(cols + 2)(9)
      nineRow +: input.map(list => 9 +: list :+ 9) :+ nineRow
    }

    def neighbours(row: Int, col: Int): List[(Int, Int)] =
      List(
        (row-1, col),
        (row+1, col),
        (row, col-1),
        (row, col+1)
      )

    def findLowPoints(): Seq[(Int, Int, Int)] = {
      for (
        row <- 1 until board.size - 1;
        col <- 1 until board(row).size - 1;
        neighbourValues = neighbours(row, col).map { case (r, c) => board(r)(c) };
        value = board(row)(col) if neighbourValues.forall(value < _)
      ) yield (value, row, col)
    }

    def collectBasin(basin: Set[(Int, Int)], collected: Set[(Int, Int)] = Set()): Set[(Int, Int)] = {
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
    .map { case (_, r, c) => board.collectBasin(Set((r, c))).size}
    .sortBy(identity)(Ordering.Int.reverse)
    .take(3)
  println(s"part 2: ${largestBasins.product}")
}
