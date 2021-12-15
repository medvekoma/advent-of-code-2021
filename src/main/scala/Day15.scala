import scala.io.Source
import scala.util.Using
import utils.mutable
import utils.immutable.MatrixExtensions._

object Day15 extends App {

  val lines = Using(Source.fromResource("day15.txt")) {
    _.getLines().toList
  }.get

  val valueMatrix = lines.map { row =>
    row.map(_.asDigit).toList
  }

  val startCell = (0, 0)
  val finishCell = (valueMatrix.rows - 1, valueMatrix.cols - 1)

  val costArray = Array.tabulate(valueMatrix.rows, valueMatrix.cols)((x, y) => Int.MaxValue)
  val costMatrix = new mutable.Matrix(costArray)

  costMatrix(startCell) = 0

  type Cell = (Int, Int)
  type Path = List[Cell]

  def findPaths(lastCell: Cell): Unit = {
    if (lastCell != finishCell) {
      val nextCells = valueMatrix.neighbours(lastCell)
        .filter(cell => costMatrix(cell) > costMatrix(lastCell) + valueMatrix(cell))

      nextCells.foreach(cell => costMatrix(cell) = costMatrix(lastCell) + valueMatrix(cell) )

      println(nextCells)

      nextCells
        .foreach(findPaths)
    }
  }

  findPaths(startCell)
  println(costMatrix(finishCell))

}
