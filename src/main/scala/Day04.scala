import scala.io.Source
import scala.util.Using

object Day04 extends App {

  type Board = List[List[Int]]

  val lines = Using(Source.fromResource("day04.txt")) { source =>
    source.getLines().toList
  }.get

  val numbers = lines.head.split(",").map(_.toInt).toList

  val boards = lines.tail.filterNot(_.isEmpty).sliding(5, 5)
    .map { board =>
      board.map(_.split(" ").filterNot(_.isEmpty).map(_.toInt).toList)
    }.toList

  val result = numbers.zipWithIndex
    .map { case (number, index) => (number, numbers.take(index + 1))}
    .map { case (number, guesses) => (guesses, boards.find(_.isBingo(guesses)))}
    .find { case (guesses, maybeBoard) => maybeBoard.isDefined }

  val solution1 = result.headOption match {
    case Some((guesses, Some(board))) =>
      board.getUnmarked(guesses).sum * guesses.last

  }

  println(s"part 1: $solution1")

  val (lastUsefulGuesses, foundBoards, _) = numbers.zipWithIndex
    .map { case (_, index) => numbers.take(index + 1)}
    .foldLeft((Seq[Int](), List[Board](), boards)) {
      case ((lastUsefulGuesses, foundBoards, remainingBoards), guesses) =>
        remainingBoards.filter(_.isBingo(guesses)) match {
          case List()
            => (lastUsefulGuesses, foundBoards, remainingBoards)
          case newBoards =>
            (guesses, foundBoards ++ newBoards, remainingBoards.filterNot(newBoards.contains(_)))
        }
    }
//  println(s"Last useful guess: ${lastUsefulGuesses}")
//  println(s"Last board: ${foundBoards.last}")
  val solution2 = foundBoards.last.getUnmarked(lastUsefulGuesses).sum * lastUsefulGuesses.last
  println(s"part 2: $solution2")


  implicit class RichBoard(board: Board) {
    def isBingo(numbers: Seq[Int]): Boolean = {
      // check rows
      val rowBingo = board.exists(row => row.forall(numbers.contains(_)))
      if (rowBingo)
        return true

      val columnarBoard = board.transpose
      val columnBingo = columnarBoard.exists(row => row.forall(numbers.contains(_)))
      columnBingo
    }

    def getUnmarked(numbers: Seq[Int]): Seq[Int] = {
      board.flatten.filterNot(numbers.contains(_))
    }
  }

}
