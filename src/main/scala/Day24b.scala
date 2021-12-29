import utils.ResourceFile

import scala.collection.{Map, mutable}

object Day24b extends App {

  val iterator = (0 to 13).iterator
  val inpPattern = "inp ([wxyz])".r
  val expressionPattern = "(\\w+) ([wxyz]) ([wxyz]?)(-?\\d*)".r

  case class Operation(op: String, reg: Char, reg2: Option[Char], value: Option[Int])

  val operations = ResourceFile.readLines("day24.txt")
    .collect {
      case inpPattern(reg) => Operation("inp", reg.head, None, Some(iterator.next()))
      case expressionPattern(op, reg, reg2, value) => Operation(op, reg.head, reg2.headOption, value.toIntOption)
    }
  case class QuantumNumber(val value: Set[Long]) {

    def add(that: QuantumNumber): QuantumNumber =
      operation(that, _ + _)

    def mul(that: QuantumNumber): QuantumNumber =
      operation(that, _ * _)

    def div(that: QuantumNumber): QuantumNumber =
      operation(that, _ / _, valid2 = _ != 0)

    def mod(that: QuantumNumber): QuantumNumber =
      operation(that, _ % _, valid1 = _ >= 0, valid2 = _ > 0)

    def eql(that: QuantumNumber): QuantumNumber =
      operation(that, (a, b) => if (a == b) 1 else 0)

    def operation(op: String): QuantumNumber => QuantumNumber =
      op match {
        case "add" => add
        case "mul" => mul
        case "div" => div
        case "mod" => mod
        case "eql" => eql
      }

    private def operation(that: QuantumNumber, fn: (Long, Long) => Long,
                          valid1: Long => Boolean = _ => true,
                          valid2: Long => Boolean = _ => true
                         ): QuantumNumber = {
      val values = for (
        v1 <- this.value;
        v2 <- that.value;
        v = fn(v1, v2) if valid1(v1) && valid2(v2)
      ) yield v
      new QuantumNumber(values)
    }
  }

  object QuantumNumber {
    def fromValue(value: Long): QuantumNumber =
      new QuantumNumber(Set(value))

    def fromInput(index: Int): QuantumNumber =
      new QuantumNumber((1L to 9L).toSet)
  }

  val registers = mutable.HashMap(
    'w' -> QuantumNumber.fromValue(0),
    'x' -> QuantumNumber.fromValue(0),
    'y' -> QuantumNumber.fromValue(0),
    'z' -> QuantumNumber.fromValue(0),
  )

  def process() {
    var i = 0
    operations.foreach { operation =>
      println(s"$i. $operation")
      i += 1
      operation match {
        case Operation("inp", reg, None, Some(index)) =>
          registers(reg) = QuantumNumber.fromInput(index)
        case Operation("mul", reg, None, Some(0)) =>
          registers(reg) = QuantumNumber.fromValue(0)
        case Operation("mul", reg, None, Some(1)) =>
        case Operation("div", reg, None, Some(1)) =>
          ;
        case Operation(op, reg, None, Some(value)) =>
          registers(reg) = registers(reg).operation(op)(QuantumNumber.fromValue(value))
        case Operation(op, reg1, Some(reg2), None) =>
          registers(reg1) = registers(reg1).operation(op)(registers(reg2))
      }
      printRegisters()
//      registers.foreach(println)
    }
  }

  def printRegisters() = {
    "wxyz".foreach { reg =>
      val value = registers(reg).value
      println(s"$reg: size: ${value.size}, min: ${value.min}, max: ${value.max}")
    }
  }

  process()
}
