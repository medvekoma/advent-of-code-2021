import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

import utils.mutable

object Day11 extends App {

  val lines = Using(Source.fromResource("day11.txt")) {
    _.getLines().toList
  }.get

  type Cell = (Int, Int)
  val array: Array[Array[Int]] = lines
    .map(line => line.toCharArray.map(_.asDigit))
    .toArray

  class Octopuses(array: Array[Array[Int]]) {

    val matrix = new mutable.Matrix(array)

    def findNeighbours(cell: Cell): Set[Cell] = {
      val (row, col) = cell
      for (
        r <- (row - 1 to row + 1).toSet & (0 until matrix.rows).toSet;
        c <- (col - 1 to col + 1).toSet & (0 until matrix.cols).toSet;
        cell = (r, c) if cell != (row, col)
      ) yield cell
    }

    def increaseCells(cells: Seq[Cell]): Unit = {
      val cellMap = cells.groupBy(identity).map { case (cell, list) => (cell, list.length) }
      cellMap.foreach {
        case (cell, count) => matrix(cell) += count
      }
    }

    def increaseAll(): Unit =
      matrix.cells.foreach(matrix(_) += 1)

    @tailrec
    private def flash(flashedCells: Set[Cell] = Set.empty): Set[Cell] = {
      val flashingCells = matrix.cells
        .filter(cell => matrix(cell) > 9)
        .toSet -- flashedCells

      if (flashingCells.isEmpty)
        flashedCells
      else {
        val neighboursOfFlashingCells = flashingCells.toList
          .flatMap(findNeighbours)

        increaseCells(neighboursOfFlashingCells)
        flash(flashedCells ++ flashingCells)
      }
    }

    def coolDown(flashedCells: Set[Cell]): Unit =
      flashedCells.foreach(cell => matrix(cell) = 0)

    def step(): Int = {
      increaseAll()
      val flashedCells = flash()
      coolDown(flashedCells)
      flashedCells.size
    }
  }

  val octopuses1 = new Octopuses(array)
  val part1 = (1 to 100).foldLeft(0) { (total, i) =>
    total + octopuses1.step()
  }
  println(s"part 1: $part1")

  val octopuses2 = new Octopuses(array)
  var part2 = 1
  while (octopuses2.step() != 100) {
    part2 += 1
  }
  println(s"part 2: $part2")
}
