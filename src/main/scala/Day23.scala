import utils.EnumerationExtensions.RichSeq
import utils.MatrixExtensions._
import utils.{Measure, ResourceFile}

object Day23 extends App {

  val lines = ResourceFile.readLines("day23.txt")
  val width = lines.head.length

  val matrix = lines.tail.take(lines.size - 2)
    .map(line => line.replace('.', ' ').padTo(width, ' ').toList)

  type Cell = (Int, Int)

  object Board {
    private val costMap: Map[Char, Int] = Map(
      'A' -> 1,
      'B' -> 10,
      'C' -> 100,
      'D' -> 1000
    )
    private val homeCols = Map('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)
    private val restPlaces = Seq(1, 2, 4, 6, 8, 10, 11).map((0, _)).toList

    def fromMatrix(matrix: List[List[Char]]): Board = {
      val cellMap = matrix.cells
        .map(cell => (cell, matrix(cell)))
        .filter { case (cell, ch) => "ABCD".contains(ch) }
        .toMap
      val homeRows = (1 until matrix.rows).toList
      val homeMap: Map[Char, List[Cell]] =
        homeCols.map { case (ch, col) => (ch, homeRows.map((_, col))) }
      Board(cellMap, cost = 0, homeMap, matrix.rows)
    }
  }

  case class Board(cellMap: Map[Cell, Char], cost: Int, homeMap: Map[Char, List[Cell]], rows: Int) {

    private def replace(source: Cell, target: Cell): Map[Cell, Char] = {
      val ch = cellMap(source)
      cellMap - source + (target -> ch)
    }

    private def pathCells(source: Cell, target: Cell): Seq[Cell] = {
      val (sourceRow, sourceCol) = source
      val (targetRow, targetCol) = target
      ((sourceRow - 1 until 0 by -1).map(row => (row, sourceCol)) ++
        (Math.min(sourceCol, targetCol) to Math.max(sourceCol, targetCol)).map(col => (0, col)) ++
        (0 to targetRow).map(row => (row, targetCol))).distinct.filterNot(_ == source)
    }

    private def isValidMove(path: Seq[Cell]): Boolean =
      contentOf(path).distinct == Seq(' ')

    trait HomeState

    case object HomeReady extends HomeState

    case class HomeStepOut(fromCell: Cell) extends HomeState

    case class HomeStepIn(toCell: Cell) extends HomeState

    def contentOf(cell: Cell): Char =
      cellMap.getOrElse(cell, ' ')

    def contentOf(cells: Seq[Cell]): Seq[Char] =
      cells.map(contentOf)

    def homeState(ch: Char): HomeState = {
      val cells = homeMap(ch)
      val content = contentOf(cells).distinct.sorted
      content match {
        case Seq(`ch`) =>
          HomeReady
        case Seq(' ', `ch`) =>
          HomeStepIn(cells.findLast(contentOf(_) == ' ').get)
        case Seq(' ') =>
          HomeStepIn(cells.last)
        case _ =>
          HomeStepOut(cells.find(contentOf(_) != ' ').get)
      }
    }

    def stepInCell(ch: Char): Option[Cell] =
      homeState(ch) match {
        case HomeStepIn(cell) =>
          Some(cell)
        case _ =>
          None
      }

    def stepOutCell(ch: Char): Option[Cell] =
      homeState(ch) match {
        case HomeStepOut(cell) =>
          Some(cell)
        case _ =>
          None
      }

    def stepInCells: Map[Char, Cell] =
      homeMap.keys
        .map(ch => (ch, stepInCell(ch)))
        .collect { case (ch, Some(cell)) => (ch, cell) }
        .toMap

    def stepOutCells: Seq[Cell] =
      homeMap.keys
        .flatMap(stepOutCell)
        .toSeq

    def isReady: Boolean =
      homeMap.keys.forall(ch => homeState(ch) == HomeReady)

    def move(source: Cell, target: Cell): Option[Board] =
      Board.costMap.get(contentOf(source)) match {
        case None => None
        case Some(pieceCost) =>
          val path = pathCells(source, target)
          if (isValidMove(path))
            Some(this.copy(cellMap = replace(source, target), cost = this.cost + pieceCost * path.size))
          else
            None
      }

    def emptyRestPlaces(cell: Cell): Seq[Cell] = {
      val (before, after) = Board.restPlaces.partition(rest => rest._2 < cell._2)
      before.takeRightWhile(contentOf(_) == ' ') ++ after.takeWhile(contentOf(_) == ' ')
    }

    def occupiedRestPlaces: Seq[Cell] =
      Board.restPlaces.filter(contentOf(_) != ' ')

    def occupiedRestPlaces(cell: Cell): Seq[Cell] = {
      val (before, after) = occupiedRestPlaces.partition(rest => rest._2 < cell._2)
      (before.lastOption ++ after.headOption).toSeq
    }

    def firstStepIn(): Option[Board] = {
      val possibleRuns = for (
        (ch, target) <- stepInCells.iterator;
        source <- occupiedRestPlaces(target) ++ stepOutCells if contentOf(source) == ch;
        board <- move(source, target)
      ) yield board
      possibleRuns.nextOption()
    }

    def allStepOuts(): Seq[Board] =
      for (
        source <- stepOutCells;
        target <- emptyRestPlaces(source);
        board <- move(source, target)
      ) yield board

    override def toString: String = {
      val cols = Board.restPlaces.map(_._2).last
      val matrix = List.tabulate(rows, cols)((r, c) => contentOf((r, c)))
      cost.toString + matrix.map(_.mkString).mkString("\n", "\n", "\n")
    }

    def findMinimumCost(): Option[Int] = {
      if (isReady)
        Some(cost)
      else firstStepIn() match {
        case Some(board) =>
          board.findMinimumCost()
        case None =>
          allStepOuts()
            .flatMap(board => board.findMinimumCost())
            .minOption
      }
    }
  }

  val board2 = Board.fromMatrix(matrix)
  val board1 = Board.fromMatrix(matrix.take(2) ++ matrix.drop(4))

  println("This will take less than a minute ...")
  Measure.dumpTime() {
    println(s"part 1: ${board1.findMinimumCost()}")
  }
  println("Please hold on ...")
  Measure.dumpTime() {
    println(s"part 2: ${board2.findMinimumCost()}")
  }
}
