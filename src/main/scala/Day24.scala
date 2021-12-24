import utils.ResourceFile

object Day24 extends App {

  val lines = ResourceFile.readLines("day24.txt")

  val (expandedLines, _) = lines.foldLeft((List.empty[String], 0)) { case ((list, index), line) =>
    val (nextLine, nextIndex) = if (line.startsWith("inp"))
      (line + s" $index", index + 1)
    else
      (line, index)
    (list :+ nextLine, nextIndex)
  }

  sealed trait Expression

  case class Number(n: Int) extends Expression

  case class Inp(reg: Char, index: Int) extends Expression

  case class Add(reg: Char, a: Expression, b: Expression) extends Expression

  case class Mul(reg: Char, a: Expression, b: Expression) extends Expression

  case class Div(reg: Char, a: Expression, b: Expression) extends Expression

  case class Mod(reg: Char, a: Expression, b: Expression) extends Expression

  case class Eql(reg: Char, a: Expression, b: Expression) extends Expression

  object Expression {
    def from(op: String, a: Expression, b: Expression): Expression =
      op match {
        case "add" => Add(' ', a, b)
        case "mul" => Add(' ', a, b)
        case "div" => Add(' ', a, b)
        case "mod" => Add(' ', a, b)
        case "eql" => Add(' ', a, b)
      }
  }

  case class Line(op: String, reg: Char, reg2: Option[Char], value: Option[Int])

  val expressionPattern = "(\\w+) ([wxyz]) ([wxyz]?)(-?\\d*)".r
  val reverseLines = expandedLines.reverse.collect {
    case expressionPattern(op, reg, reg2, value) => Line(op, reg.head, reg2.headOption, value.toIntOption)
  }

  def findExpression(from: Int, reg: Char): Expression = {
    //    println(s" -- $from, $reg")
    if (from >= reverseLines.length)
      return Number(0)
    val remainingLines = reverseLines.zipWithIndex.drop(from)
    val result = remainingLines.find { case (line, _) => line.reg == reg } match {
      case None => Number(0)
      case Some((line, index)) =>
        if (index < from)
          println(s"ERROR: $line, $from, $index")
        line match {
          case Line("inp", _, None, Some(number)) =>
            println(s"--> Input $reg, $number, $index")
            Inp(reg, number)
          case Line(op, _, None, Some(number)) =>
            val a = findExpression(index + 1, reg)
            val b = Number(number)
            Expression.from(op, a, b)
          case Line(op, _, Some(reg2), None) =>
            val a = findExpression(index + 1, reg)
            val b = findExpression(index + 1, reg2)
            Expression.from(op, a, b)
        }
    }
    //    println(s" -- $from, $reg, $result")
    result
  }

  val tree = findExpression(0, 'z')

}
