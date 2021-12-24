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

  sealed trait Operation extends Expression {
    val reg: Char
    val a: Expression
    val b: Expression
  }

  case class Add(reg: Char, a: Expression, b: Expression) extends Operation
  case class Mul(reg: Char, a: Expression, b: Expression) extends Operation
  case class Div(reg: Char, a: Expression, b: Expression) extends Operation
  case class Mod(reg: Char, a: Expression, b: Expression) extends Operation
  case class Eql(reg: Char, a: Expression, b: Expression) extends Operation

  object Expression {
    def from(op: String, reg: Char, a: Expression, b: Expression): Expression = {
      (op, b) match {
        case ("mul", Number(0)) => Number(0)
        case ("add", _) => Add(reg, a, b)
        case ("mul", _) => Mul(reg, a, b)
        case ("div", _) => Div(reg, a, b)
        case ("mod", _) => Mod(reg, a, b)
        case ("eql", _) => Eql(reg, a, b)
      }
    }
  }

  case class Line(op: String, reg: Char, reg2: Option[Char], value: Option[Int])

  val expressionPattern = "(\\w+) ([wxyz]) ([wxyz]?)(-?\\d*)".r
  val reverseLines = expandedLines.collect {
    case expressionPattern(op, reg, reg2, value) => Line(op, reg.head, reg2.headOption, value.toIntOption)
  }

  def findExpression(from: Int, reg: Char): Expression = {
    val remainingLines = reverseLines.zipWithIndex.take(from)
    val result = remainingLines.findLast { case (line, _) => line.reg == reg } match {
      case None => Number(0)
      case Some((line, index)) =>
        line match {
          case Line("inp", _, None, Some(number)) =>
            Inp(reg, number)
          case Line(op, regA, None, Some(number)) =>
            val a = findExpression(index - 1, regA)
            val b = Number(number)
            Expression.from(op, reg, a, b)
          case Line(op, regA, Some(regB), None) =>
            val a = findExpression(index - 1, regA)
            val b = findExpression(index - 1, regB)
            Expression.from(op, reg, a, b)
        }
    }
    result
  }

  val tree = findExpression(reverseLines.length, 'z')

  def findInp(expression: Expression): Seq[Inp] = {
    expression match {
      case inp: Inp => Seq(inp)
      case op: Operation => findInp(op.a) ++ findInp(op.b)
      case _ => Seq.empty
    }
  }

  println(tree)
  println(findInp(tree).distinct)

}
