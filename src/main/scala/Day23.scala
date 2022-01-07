import utils.MatrixExtensions._
import utils.{Measure, ResourceFile}

object Day23 extends App {

  val lines = ResourceFile.readLines("day23.txt")
  val width = lines.head.length

  val matrix = lines.tail.take(lines.size - 2)
    .map(line => line.replace('.', ' ').padTo(width, ' ').toList)

  type Cell = (Int, Int)

  class Board(matrix: List[List[Char]], val cost: Int = 0) {

    private val costMap: Map[Char, Int] = Map(
      'A' -> 1,
      'B' -> 10,
      'C' -> 100,
      'D' -> 1000
    )

    private val homeRows = (1 until matrix.rows).toList
    private val homeCols = Map('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)
    private val homeMap: Map[Char, List[Cell]] =
      homeCols.map { case (ch, col) => (ch, homeRows.map((_, col))) }

    private val restPlaces = Seq(1, 2, 4, 6, 8, 10, 11).map((0, _)).toList

    private def replace(source: Cell, target: Cell): List[List[Char]] = {
      List.tabulate[Char](matrix.rows, matrix.cols) {
        case `source` => matrix(target)
        case `target` => matrix(source)
        case x => matrix(x)
      }
    }

    private def pathCells(cell1: Cell, cell2: Cell): Set[Cell] =
      (cell1._1 until 0 by -1).map(row => (row, cell1._2)).toSet ++
        (Math.min(cell1._2, cell2._2) to Math.max(cell1._2, cell2._2)).map(col => (0, col)).toSet ++
        (0 to cell2._1).map(row => (row, cell2._2)).toSet -- Set(cell1)

    private def isValidMove(path: Set[Cell]): Boolean =
      path.forall(cell => matrix(cell) == ' ')

    trait HomeState

    case object HomeReady extends HomeState

    case class HomeStepOut(fromCell: Cell) extends HomeState

    case class HomeStepIn(toCell: Cell) extends HomeState

    def homeState(ch: Char): HomeState = {
      val cells = homeMap(ch)
      val content = contentOf(cells).distinct.sorted.toList
      content match {
        case List(`ch`) =>
          HomeReady
        case List(' ', `ch`) =>
          HomeStepIn(cells.findLast(matrix(_) == ' ').get)
        case List(' ') =>
          HomeStepIn(cells.last)
        case _ =>
          HomeStepOut(cells.find(matrix(_) != ' ').get)
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

    def stepOutCells: Set[Cell] =
      homeMap.keySet
        .flatMap(stepOutCell)


    def isReady: Boolean =
      homeMap.keySet.forall(ch => homeState(ch) == HomeReady)

    def contentOf(cells: Seq[Cell]): Seq[Char] =
      cells.map(matrix(_))

    def move(source: Cell, target: Cell): Option[Board] =
      costMap.get(matrix(source)) match {
        case None => None
        case Some(pieceCost) =>
          val path = pathCells(source, target)
          if (isValidMove(path))
            Some(new Board(replace(source, target), cost + pieceCost * path.size))
          else
            None
      }

    def emptyRestPlaces: Seq[Cell] =
      restPlaces.filter(matrix(_) == ' ')

    def occupiedRestPlaces: Seq[Cell] =
      restPlaces.filter(matrix(_) != ' ')

    def firstStepIn(): Option[Board] = {
      val possibleRuns = for (
        source <- (occupiedRestPlaces ++ stepOutCells).iterator;
        ch = matrix(source);
        target <- stepInCell(ch);
        board <- move(source, target)
      ) yield board
      possibleRuns.nextOption()
    }

    def allStepOuts(): Seq[Board] = {
      val boards = for (
        source <- stepOutCells;
        target <- emptyRestPlaces;
        board <- move(source, target)
      ) yield board
      boards.toSeq
    }

    override def toString: String =
      cost.toString + matrix.map(_.mkString).mkString("\n", "\n", "\n")
  }

  def findMinimumCost(board: Board): Option[Int] =
    if (board.isReady)
      Some(board.cost)
    else board.firstStepIn() match {
      case Some(newBoard) =>
        findMinimumCost(newBoard)
      case None =>
        board.allStepOuts()
          .flatMap(newBoard => findMinimumCost(newBoard))
          .minOption
    }

  val board2 = new Board(matrix)
  val board1 = new Board(matrix.take(2) ++ matrix.drop(4))

  println("This will take about two minutes ...")
  Measure.dumpTime() {
    println(s"part 1: ${findMinimumCost(board1)}")
  }
  println("Another minute to go ...")
  Measure.dumpTime() {
    println(s"part 2: ${findMinimumCost(board2)}")
  }
}
