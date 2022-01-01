import utils.ResourceFile

import scala.util.Try

object Day24 extends App {
  val lines = ResourceFile.readLines("day24.txt")
  val iterator = (13 to 0 by -1).iterator

  val operations = lines
    .map(_.split(" "))
    .map(array => (array(0), array(1), Try(array(2)).getOrElse(iterator.next().toString)))

  case class Registers(w: Long, x: Long, y: Long, z: Long) {
    def get(value: String): Long = value match {
      case "w" => w
      case "x" => x
      case "y" => y
      case "z" => z
      case v => v.toLong
    }

    def set(reg: String, value: Long): Registers = reg match {
      case "w" => copy(w = value)
      case "x" => copy(x = value)
      case "y" => copy(y = value)
      case "z" => copy(z = value)
      case _ => throw new RuntimeException(s"Incorrect registry: $reg")
    }
  }

  var cycle = 0L

  def findFirst(line: Int, registers: Registers, inputs: String): Option[String] = {
    if (line >= operations.size)
      return if (registers.z == 0L) Some(inputs) else None

    def binary(
                fn: (Long, Long) => Long,
                valid1: Long => Boolean = _ => true,
                valid2: Long => Boolean = _ => true
              )(
                a: Long, b: Long,
              ): Option[Long] = {
      if (valid1(a) && valid2(b))
        Some(fn(a, b))
      else
        None
    }

    def add = binary(_ + _) _

    def mul = binary(_ * _) _

    def div = binary(_ + _, valid2 = _ != 0) _

    def mod = binary(_ % _, valid1 = _ >= 0, valid2 = _ > 0) _

    def eql = binary((a, b) => if (a == b) 1L else 0L) _

    val operation = operations(line)
    cycle += 1
    if (cycle % 1000000 == 0)
      println(line, inputs)
    operation match {
      case ("inp", reg, _) =>
        (9 to 1 by -1).iterator
          .map(i => findFirst(line + 1, registers.set(reg, i.toLong), inputs + i))
          .find(_.isDefined).flatten
      case ("add", reg, x) =>
        add(registers.get(reg), registers.get(x))
          .flatMap(res => findFirst(line + 1, registers.set(reg, res), inputs))
      case ("mul", reg, x) =>
        mul(registers.get(reg), registers.get(x))
          .flatMap(res => findFirst(line + 1, registers.set(reg, res), inputs))
      case ("div", reg, x) =>
        div(registers.get(reg), registers.get(x))
          .flatMap(res => findFirst(line + 1, registers.set(reg, res), inputs))
      case ("mod", reg, x) =>
        mod(registers.get(reg), registers.get(x))
          .flatMap(res => findFirst(line + 1, registers.set(reg, res), inputs))
      case ("eql", reg, x) =>
        eql(registers.get(reg), registers.get(x))
          .flatMap(res => findFirst(line + 1, registers.set(reg, res), inputs))
    }
  }

  val result = findFirst(0, Registers(0, 0, 0, 0), "")
  println(result)
}