import utils.MatrixExtensions._
import utils.{Measure, ResourceFile}

import scala.collection.mutable

object Day23 extends App {

  val lines = ResourceFile.readLines("day23a.txt")
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

    private def replace(cell1: Cell, cell2: Cell): List[List[Char]] = {
      List.tabulate[Char](matrix.rows, matrix.cols) {
        case `cell1` => matrix(cell2)
        case `cell2` => matrix(cell1)
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

    def stepInCells: Set[Cell] = {
      homeMap.keySet
        .map(ch => homeState(ch))
        .collect{ case HomeStepIn(cell) => cell }
    }

    def stepOutCells: Set[Cell] = {
      homeMap.keySet
        .map(ch => homeState(ch))
        .collect{ case HomeStepOut(cell) => cell }
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

    def isReady: Boolean =
      homeMap.keySet.forall(ch => homeState(ch) == HomeReady)

    def contentOf(cells: Seq[Cell]): Seq[Char] =
      cells.map(matrix(_))

    def homeContent(ch: Char): String =
      contentOf(homeMap(ch)).mkString

    def step(source: Cell, target: Cell): Option[Board] = {
      costMap.get(matrix(source)) match {
        case None => None
        case Some(pieceCost) =>
          val path = pathCells(source, target)
          if (isValidMove(path))
            Some(new Board(replace(source, target), cost + pieceCost * path.size))
          else
            None
      }
    }

    def nextHome(ch: Char): Option[Cell] = {
      val cells = homeMap(ch)
      if ((contentOf(cells).toSet -- Set(ch, ' ')).isEmpty)
        cells.findLast(matrix(_) == ' ')
      else
        None
    }

    def noStepOut(ch: Char): Boolean = {
      (contentOf(homeMap(ch)).toSet -- Set(ch, ' ')).isEmpty
    }

    def emptyRestPlaces: Seq[Cell] =
      restPlaces.filter(matrix(_) == ' ')

    def occupiedRestPlaces: Seq[Cell] =
      restPlaces.filter(matrix(_) != ' ')

    def firstHomeRun(): Option[Board] = {
      val possibleRuns = for (
        source <- occupiedRestPlaces.iterator;
        ch = matrix(source);
        target <- stepInCell(ch);
        board <- step(source, target)
      ) yield board
      possibleRuns.nextOption()
    }

    def findDirectHomeRun(): Option[Board] = {
      val possibleRuns = for (
        source <- stepOutCells.iterator;
        ch = matrix(source);
        target <- stepInCell(ch);
        board <- step(source, target)
      ) yield board
      possibleRuns.nextOption()
    }

    def findStepOuts(): Seq[Board] = {
      val res = for (
        source <- stepOutCells;
        target <- emptyRestPlaces;
        board <- step(source, target)
      ) yield board
      res.toSeq
    }

    override def toString: String =
      cost.toString + matrix.map(_.mkString).mkString("\n", "\n", "\n")
  }

  val board = new Board(matrix)
//  val costs = mutable.ListBuffer[Int]()
  var minCost = Int.MaxValue

  def findSolution(board: Board): Unit = {
//    println(board)
    if (board.isReady) {
      minCost = math.min(minCost, board.cost)
//      costs += board.cost
    } else board.firstHomeRun() match {
      case Some(newBoard) =>
        findSolution(newBoard)
      case None =>
        board.findDirectHomeRun() match {
          case Some(newBoard) =>
            findSolution(newBoard)
          case None =>
            board.findStepOuts().foreach(findSolution)
        }
    }
  }

  Measure.dumpTime() {
    findSolution(board)
  }
  Measure.dumpTime() {
    println(minCost)
//    println(s"min: ${costs.min}; count: ${costs.size}")
  }
}
