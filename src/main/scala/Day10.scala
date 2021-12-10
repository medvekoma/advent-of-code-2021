import scala.collection.mutable
import scala.io.Source
import scala.util.{Success, Try, Using}

object Day10 extends App {

  val lines = Using(Source.fromResource("day10.txt")) {
    _.getLines().toList
  }.get

  sealed trait Result

  case object Correct extends Result

  case class Corrupted(char: Char) extends Result {
    val costMap = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )

    def cost: Int = costMap.getOrElse(char, 0)
  }

  case class Incomplete(missing: String) extends Result {
    val costMap = Map(
      ')' -> 1,
      ']' -> 2,
      '}' -> 3,
      '>' -> 4
    )

    def cost: Long =
      missing.foldLeft(0L) { (total, char) =>
        total * 5 + costMap.getOrElse(char, 0)
      }
  }

  class Parser(line: String) {

    val stack: mutable.Stack[Char] = mutable.Stack[Char]()

    val bracketMap = Map(
      '{' -> '}',
      '(' -> ')',
      '[' -> ']',
      '<' -> '>'
    )

    def parse(): Result = {
      line.foreach { current =>
        bracketMap.get(current) match {
          case Some(closing) =>
            stack.push(closing)
          case None =>
            if (Try(stack.pop()) != Success(current)) return Corrupted(current)
        }
      }
      if (stack.isEmpty)
        Correct
      else
        Incomplete(stack.mkString)
    }
  }

  val results = lines
    .map(line => new Parser(line).parse())

  val part1 = results
    .collect { case res: Corrupted => res.cost }
    .sum

  println(s"part 1: $part1")

  val part2 = results
    .collect { case res: Incomplete => res.cost }
    .sorted

  println(s"part 2: ${part2(part2.size / 2)}")
}
