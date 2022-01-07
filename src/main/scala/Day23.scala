import utils.MatrixExtensions._
import utils.{Measure, ResourceFile}

object Day23 extends App {

  val lines = ResourceFile.readLines("day23.txt")
  val width = lines.head.length

  val matrix = lines.tail.take(lines.size - 2)
    .map(line => line.replace('.', ' ').padTo(width, ' ').toList)

  type Cell = (Int, Int)

  object Board {
    def fromMatrix(matrix: List[List[Char]]): Board = {
      new Board(matrix, 0)
    }
  }

  class Board(matrix: Seq[Seq[Char]], val cost: Int) {

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

    private def replace(source: Cell, target: Cell): Seq[Seq[Char]] = {
      val sourceCh = matrix(source)
      val targetCh = matrix(target)
      matrix.update(source, targetCh).update(target, sourceCh)
    }

    private def pathCells(source: Cell, target: Cell): Set[Cell] =
      (source._1 - 1 until 0 by -1).map(row => (row, source._2)).toSet ++
        (Math.min(source._2, target._2) to Math.max(source._2, target._2)).map(col => (0, col)).toSet ++
        (0 to target._1).map(row => (row, target._2)).toSet -- Set(source)

    private def isValidMove(path: Set[Cell]): Boolean =
      contentOf(path.toSeq).toSet == Set(' ')

  trait HomeState

    case object HomeReady extends HomeState

    case class HomeStepOut(fromCell: Cell) extends HomeState

    case class HomeStepIn(toCell: Cell) extends HomeState

    def contentOf(cell: Cell): Char =
      matrix(cell)

    def contentOf(cells: Seq[Cell]): Seq[Char] =
      cells.map(contentOf)

    def homeState(ch: Char): HomeState = {
      val cells = homeMap(ch)
      val contentList = contentOf(cells).distinct.sorted.toList
      contentList match {
        case List(`ch`) =>
          HomeReady
        case List(' ', `ch`) =>
          HomeStepIn(cells.findLast(contentOf(_) == ' ').get)
        case List(' ') =>
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
      homeMap.keySet
        .map(ch => (ch, stepInCell(ch)))
        .collect { case (ch, Some(cell)) => (ch, cell) }
        .toMap

    def stepOutCells: Set[Cell] =
      homeMap.keySet
        .flatMap(stepOutCell)

    def isReady: Boolean =
      homeMap.keySet.forall(ch => homeState(ch) == HomeReady)

    def move(source: Cell, target: Cell): Option[Board] =
      costMap.get(contentOf(source)) match {
        case None => None
        case Some(pieceCost) =>
          val path = pathCells(source, target)
          if (isValidMove(path))
            Some(new Board(replace(source, target), cost + pieceCost * path.size))
          else
            None
      }

    def emptyRestPlaces(cell: Cell): Seq[Cell] = {
      val (before, after) = restPlaces.partition(rest => rest._2 < cell._2)
      before.reverse.takeWhile(contentOf(_) == ' ') ++ after.takeWhile(contentOf(_) == ' ')
    }

    def occupiedRestPlaces: Seq[Cell] =
      restPlaces.filter(contentOf(_) != ' ')

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

    def allStepOuts(): Seq[Board] = {
      val boards = for (
        source <- stepOutCells;
        target <- emptyRestPlaces(source);
        board <- move(source, target)
      ) yield board
      boards.toSeq
    }

    override def toString: String = {
      cost.toString + matrix.map(_.mkString).mkString("\n", "\n", "\n")
    }

    def findMinimumCost(): Option[Int] = {
//      println(this)
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

  println("This will take about a minute ...")
  Measure.dumpTime() {
    println(s"part 1: ${board1.findMinimumCost()}")
  }
  println("Please hold on ...")
  Measure.dumpTime() {
    println(s"part 2: ${board2.findMinimumCost()}")
  }
}
