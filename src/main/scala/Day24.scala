import utils.ResourceFile

import scala.collection.mutable

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
      (op, a, b) match {
        case ("add", Number(n1), Number(n2)) => Number(n1 + n2)
        case ("mul", Number(n1), Number(n2)) => Number(n1 * n2)
        case ("div", Number(n1), Number(n2)) => Number(n1 / n2)
        case ("mod", Number(n1), Number(n2)) => Number(n1 % n2)
        case ("eql", Number(n1), Number(n2)) => Number(if (n1 == n2) 1 else 0)
        case ("mul", _, Number(0)) => Number(0)
        case ("mul", _, Number(1)) => a
        case ("div", _, Number(1)) => a
        case ("add", _, Number(0)) => a
        case ("mul", Number(0), _) => Number(0)
        case ("mul", Number(1), _) => b
        case ("add", Number(0), _) => b
        case ("add", _, _) => Add(a, b)
        case ("mul", _, _) => Mul(a, b)
        case ("div", _, _) => Div(a, b)
        case ("mod", _, _) => Mod(a, b)
        case ("eql", _, _) => Eql(a, b)
      }
    }
  }

  case class Operation(op: String, reg: Char, reg2: Option[Char], value: Option[Int])

  val expressionPattern = "(\\w+) ([wxyz]) ([wxyz]?)(-?\\d*)".r
  val operations = expandedLines.collect {
    case expressionPattern(op, reg, reg2, value) => Operation(op, reg.head, reg2.headOption, value.toIntOption)
  }

  val registers: mutable.Map[Char, Expression] = mutable.HashMap()

  operations.foreach {
    case Operation("inp", reg, None, Some(number)) =>
      registers(reg) = Inp(number)
    case Operation(op, reg, None, Some(number)) =>
      val a = registers.getOrElse(reg, Number(0))
      registers(reg) = Expression.from(op, a, Number(number))
    case Operation(op, reg1, Some(reg2), None) =>
      val a = registers.getOrElse(reg1, Number(0))
      val b = registers.getOrElse(reg2, Number(0))
      registers(reg1) = Expression.from(op, a, b)
  }

  val tree = registers('z')

  val registerValues = mutable.HashMap(
    'w' -> mutable.HashMap(0L -> List.empty),
    'x' -> mutable.HashMap(0L -> List.empty),
    'y' -> mutable.HashMap(0L -> List.empty),
    'z' -> mutable.HashMap(0L -> List.empty)
  )
  val wrongValues = mutable.ListBuffer[(Int, Int)]()

//  def parseConditions(expression: Expression)

//  def findInp(expression: Expression): Seq[Inp] = {
//    expression match {
//      case inp: Inp => Seq(inp)
//      case op: Binary => findInp(op.a) ++ findInp(op.b)
//      case _ => Seq.empty
//    }
//  }
//
//  println(tree)
//  println(findInp(tree).distinct)

}
