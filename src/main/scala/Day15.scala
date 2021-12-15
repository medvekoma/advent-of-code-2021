import utils.MatrixExtensions._
import utils.MutableMatrix

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day15 extends App {

  val lines = Using(Source.fromResource("day15.txt")) {
    _.getLines().toList
  }.get

  val matrix1 = lines.map { row =>
    row.map(_.asDigit).toList
  }

  class Dijkstra(matrix: List[List[Int]]) {
    val startCell: (Int, Int) = (0, 0)
    val finishCell: (Int, Int) = (matrix.rows - 1, matrix.cols - 1)

    var unvisited: Set[(Int, Int)] = matrix.cells.toSet
    val costMatrix = new MutableMatrix(Array.tabulate(matrix.rows, matrix.cols)((x, y) => Int.MaxValue))
    val unvisitedCellsWithCost = new mutable.HashMap[(Int, Int), Int]()

    def updateCost(cell: (Int, Int), value: Int): Unit =
      if (value < costMatrix(cell)) {
        costMatrix(cell) = value
        unvisitedCellsWithCost(cell) = value
      }

    updateCost(startCell, 0)

    def popNextUnvisited(): Option[((Int, Int), Int)] = {
      unvisitedCellsWithCost.minByOption { case (cell, value) => value } match {
        case result @ Some((cell, value)) =>
          unvisited -= cell
          unvisitedCellsWithCost.remove(cell)
          result
        case _ =>
          None
      }
    }

    @tailrec
    final def run(): Unit = {
      if (unvisited.size % 50000 == 0)
        println(s"  ... unvisited: ${unvisited.size}")
      popNextUnvisited() match {
        case Some((cell, value)) if cell != finishCell =>
          (matrix.neighbours(cell) & unvisited)
            .foreach(next => updateCost(next, value + matrix(next)))
          run()
        case _ =>
      }
    }

    def finishValue: Int = costMatrix(finishCell)
  }

  val dijkstra1 = new Dijkstra(matrix1)
  dijkstra1.run()
  println(s"part 1: ${dijkstra1.finishValue}")

  def enlarge(matrix: List[List[Int]]): List[List[Int]] =
    Array.tabulate(matrix.rows * 5, matrix.cols * 5) { (r, c) =>
      val page = r / matrix.rows + c / matrix.cols
      matrix(r % matrix.rows)(c % matrix.cols) + page match {
        case v if v > 9 => v - 9
        case v => v
      }
    }.map(_.toList).toList

  val matrix2 = enlarge(matrix1)
  val dijkstra2 = new Dijkstra(matrix2)
  dijkstra2.run()
  println(s"part 2: ${dijkstra2.finishValue}")

}
