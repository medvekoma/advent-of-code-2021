import utils.ResourceFile

import scala.collection.mutable

object Day24 extends App {

  val iterator = (0 to 13).iterator
  val inpPattern = "inp ([wxyz])".r
  val expressionPattern = "(\\w+) ([wxyz]) ([wxyz]?)(-?\\d*)".r

  case class Operation(op: String, reg: Char, reg2: Option[Char], value: Option[Int])

  val operations = ResourceFile.readLines("day24.txt")
    .collect {
      case inpPattern(reg) => Operation("inp", reg.head, None, Some(iterator.next()))
      case expressionPattern(op, reg, reg2, value) => Operation(op, reg.head, reg2.headOption, value.toIntOption)
    }

  class QuantumNumber(val value: Map[Option[Int], List[Set[Int]]]) {
    override def toString: String =
      s"QuantumNumber(${value.mkString("\n  ", "\n  ", "\n")})"

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

    private def operation(that: QuantumNumber, fn: (Int, Int) => Int,
                          valid1: Int => Boolean = _ => true,
                          valid2: Int => Boolean = _ => true
                         ): QuantumNumber = {
      val values = for (
        (v1, c1) <- this.value.toList;
        (v2, c2) <- that.value.toList
      ) yield (v1, v2, c1, c2)
      val value = values.map {
        case (Some(v1), Some(v2), c1, c2) if valid1(v1) && valid2(v2) =>
          (Some(fn(v1, v2)), c1.zip(c2).map { case (a, b) => a & b })
        case (Some(v1), Some(v2), c1, c2) if !valid1(v1) =>
          (None, c1)
        case (Some(v1), Some(v2), c1, c2) if !valid2(v2) =>
          (None, c2)
        case (_, _, c1, c2) =>
          (None, c1.zip(c2).map { case (a, b) => a ++ b })
      }.groupMap(_._1)(_._2)
        .map { case (value, list) => (value, list.transpose.map(c => c.reduce(_ ++ _))) }
        .filter { case (value, constraints) => constraints.forall(_.nonEmpty)}
      new QuantumNumber(value)
    }
  }

  object QuantumNumber {
    private val digits: Set[Int] = (1 to 9).toSet
    private val always: List[Set[Int]] = List.fill(14)(digits)

    def fromValue(value: Int): QuantumNumber =
      new QuantumNumber(Map(Some(value) -> always))

    def fromInput(index: Int): QuantumNumber = {
      val value = (1 to 9)
        .map(i => Option(i) -> always.patch(index, List(Set(i)), 1))
        .toMap
      new QuantumNumber(value)
    }
  }

  val v0 = QuantumNumber.fromInput(0)
  val v1 = QuantumNumber.fromInput(1)
  println (v0)
  println (v1)
  println (v0 eql v1)

  def process() {
    val registers = mutable.HashMap(
      'w' -> QuantumNumber.fromValue(0),
      'x' -> QuantumNumber.fromValue(0),
      'y' -> QuantumNumber.fromValue(0),
      'z' -> QuantumNumber.fromValue(0),
    )

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
    }
    println(registers('z'))
  }

//  process()
}
