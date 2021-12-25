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

    def div(that: QuantumNumber): QuantumNumber =
      operation(that, _ / _)

    private def operation(that: QuantumNumber, fn: (Int, Int) => Int): QuantumNumber = {
      val values = for (
        (v1, c1) <- this.value.toList;
        (v2, c2) <- that.value.toList
      ) yield (v1, v2, c1, c2)
      val value = values.map {
        case (Some(v1), Some(v2), c1, c2) =>
          (Some(fn(v1, v2)), c1.zip(c2).map { case (a, b) => a & b })
        case (_, _, c1, c2) =>
          (None, c1.zip(c2).map { case (a, b) => a ++ b })
      }.groupMap(_._1)(_._2)
        .map{ case (value, list) => (value, list.transpose.map(c => c.reduce(_ ++ _)))}
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

  val q1 = QuantumNumber.fromInput(0)
  val q2 = QuantumNumber.fromValue(2)
  val q = q1 div q2

  println(q1)
  println(q2)
  println(q)
}
