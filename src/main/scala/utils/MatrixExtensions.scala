package utils

object MatrixExtensions {

  implicit class Matrix[T](matrix: List[List[T]]) {
    val rows: Int = matrix.length
    val cols: Int = matrix.headOption.map(_.length).getOrElse(0)

    def neighbours(cell: (Int, Int)): Set[(Int, Int)] = {
      val (row, col) = cell
      Set(
        (row - 1, col),
        (row + 1, col),
        (row, col - 1),
        (row, col + 1)
      )
        .filter { case (r, c) => r >= 0 && r < rows && c >= 0 && c < cols }
    }

    def apply(cell: (Int, Int)): T =
      matrix(cell._1)(cell._2)

    def cells: Seq[(Int, Int)] =
      for (
        r <- matrix.indices;
        c <- matrix(r).indices
      )
      yield (r, c)
  }
}
