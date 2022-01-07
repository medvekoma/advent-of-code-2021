package utils

class MutableMatrix[T](init: Array[Array[T]]) {
  val rows: Int = init.length
  val cols: Int = init.headOption.map(_.length).getOrElse(0)
  private val wrapped = init.transpose.transpose

  def cells: Seq[(Int, Int)] =
    for (
      r <- wrapped.indices;
      c <- wrapped(r).indices
    )
    yield (r, c)

  def apply(cell: (Int, Int)): T =
    wrapped(cell._1)(cell._2)

  def update(cell: (Int, Int), value: T): Unit =
    wrapped(cell._1)(cell._2) = value

  override def toString: String =
    wrapped.map(row => row.mkString(" "))
      .mkString("\n")
}
