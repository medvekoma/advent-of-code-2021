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

  case class Number(n: Int) extends Expression {
    override def toString: String = n.toString
  }

  case class Inp(index: Int) extends Expression {
    override def toString: String = s"input($index).asDigit"
  }

  sealed trait Binary extends Expression {
    val a: Expression
    val b: Expression
  }

  case class Add(a: Expression, b: Expression) extends Binary {
    override def toString: String = s"($a + $b)"
  }
  case class Mul(a: Expression, b: Expression) extends Binary {
    override def toString: String = s"($a * $b)"
  }
  case class Div(a: Expression, b: Expression) extends Binary {
    override def toString: String = s"($a / $b)"
  }
  case class Mod(a: Expression, b: Expression) extends Binary {
    override def toString: String = s"($a % $b)"
  }
  case class Eql(a: Expression, b: Expression) extends Binary {
    override def toString: String = s"(if ($a == $b) 1 else 0)"
  }

  object Expression {
    def from(op: String, a: Expression, b: Expression): Expression = {
      (op, b) match {
//        case ("mul", Number(0)) => Number(0)
//        case ("mul", Number(1)) => a
//        case ("div", Number(1)) => a
//        case ("add", Number(0)) => a
        case ("add", _) => Add(a, b)
        case ("mul", _) => Mul(a, b)
        case ("div", _) => Div(a, b)
        case ("mod", _) => Mod(a, b)
        case ("eql", _) => Eql(a, b)
      }
    }
  }

  case class Operation(op: String, reg: Char, reg2: Option[Char], value: Option[Int])

  val expressionPattern = "(\\w+) ([wxyz]) ([wxyz]?)(-?\\d*)".r
  val operations = expandedLines.collect {
    case expressionPattern(op, reg, reg2, value) => Operation(op, reg.head, reg2.headOption, value.toIntOption)
  }

  def findExpression(lineCount: Int, reg: Char): Expression = {
    val lastOperation = operations.zipWithIndex
      .take(lineCount)
      .findLast { case (operation, _) => operation.reg == reg }
    println(s"{{{ $lastOperation")
    val result = lastOperation match {
      case Some((operation, index)) =>
        operation match {
          case Operation("inp", _, None, Some(number)) =>
            Inp(number)
          case Operation(op, regA, None, Some(number)) =>
            println(s"... find: $index, $regA")
            val a = findExpression(index, regA)
            val b = Number(number)
            Expression.from(op, a, b)
          case Operation(op, regA, Some(regB), None) =>
            println(s"... find: $index, $regA")
            val a = findExpression(index, regA)
            println(s"... find: $index, $regB")
            val b = findExpression(index, regB)
            Expression.from(op, a, b)
        }
      case None =>
        Number(0)
    }
    println(s"}}} $lastOperation, ${result.getClass.getSimpleName}")
    result
  }

  val tree = findExpression(operations.length, 'z')

  def findInp(expression: Expression): Seq[Inp] = {
    expression match {
      case inp: Inp => Seq(inp)
      case op: Binary => findInp(op.a) ++ findInp(op.b)
      case _ => Seq.empty
    }
  }

  println(tree)
  println(findInp(tree).distinct)

}
