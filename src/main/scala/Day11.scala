import utils.Matrix

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day11 extends App {

  val lines = Using(Source.fromResource("day11.txt")) {
    _.getLines().toList
  }.get

  type Cell = (Int, Int)
  val input = lines
    .map(line => line.toCharArray.map(_.asDigit))
    .toArray

  class Octopuses(matrix: Matrix[Int]) {

    def findNeighbours(cell: Cell): Set[Cell] = {
      val (row, col) = cell
      for (
        r <- (row - 1 to row + 1).toSet & (0 until matrix.rows).toSet;
        c <- (col - 1 to col + 1).toSet & (0 until matrix.cols).toSet;
        cell = (r, c) if cell != (row, col)
      ) yield cell
    }

    def increase(cells: Seq[Cell]): Unit = {
      val cellMap = cells.groupBy(identity).map { case (cell, list) => (cell, list.length) }
      cellMap.foreach {
        case (cell, value) => matrix(cell) += value
      }
    }

    def increaseAll(): Unit =
      matrix.cells.foreach(matrix(_) += 1)

    @tailrec
    private def flash(flashed: Set[Cell] = Set.empty): Set[Cell] = {
      val activated = matrix.cells
        .filter(cell => matrix(cell) > 9)
        .toSet -- flashed

      if (activated.isEmpty)
        flashed
      else {
        val activatedNeighbours = activated.toList
          .flatMap(findNeighbours)

        increase(activatedNeighbours)
        flash(flashed ++ activated)
      }
    }

    def coolDown(flashed: Set[Cell]): Unit =
      flashed.foreach(matrix(_) = 0)

    def step(): Int = {
      increaseAll()
      val flashed = flash()
      coolDown(flashed)
      flashed.size
    }
  }

  val octopuses1 = new Octopuses(new Matrix(input))
  val part1 = (1 to 100).foldLeft(0) { (total, i) =>
      total + octopuses1.step()
  }
  println(s"part 1: $part1")

  val octopuses2 = new Octopuses(new Matrix(input))
  var part2 = 1
  while (octopuses2.step() != 100) {
    part2 += 1
  }
  println(s"part 2: $part2")
}
