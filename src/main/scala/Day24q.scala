import utils.{Measure, ResourceFile}

import scala.collection.{Set, mutable}
import scala.util.Try

object Day24q extends App {

  val iterator = (13 to 0 by -1).iterator

  val operations = ResourceFile.readLines("day24.txt")
    .map(_.split(" "))
    .map(arr => (arr(0), arr(1), Try(arr(2)).getOrElse(iterator.next().toString)))

  class QuantumNumber(val set: Set[Long]) {
    private def binary(that: QuantumNumber,
                          fn: (Long, Long) => Long,
                          valid1: Long => Boolean = _ => true,
                          valid2: Long => Boolean = _ => true
                         ): QuantumNumber = {
      val internalValues = for (
        v1 <- this.set;
        v2 <- that.set;
        v = fn(v1, v2) if valid1(v1) && valid2(v2)
      ) yield v
      new QuantumNumber(internalValues)
    }

    def add(that: QuantumNumber): QuantumNumber = binary(that, _ + _)
    def mul(that: QuantumNumber): QuantumNumber = binary(that, _ * _)
    def div(that: QuantumNumber): QuantumNumber = binary(that, _ / _, valid2 = _ != 0)
    def mod(that: QuantumNumber): QuantumNumber = binary(that, _ % _, valid1 = _ >= 0, valid2 = _ > 0)
    def eql(that: QuantumNumber): QuantumNumber = binary(that, (a, b) => if (a == b) 1L else 0L)
  }

  object QuantumNumber {
    def fromNumber(n: Long): QuantumNumber = new QuantumNumber(Set(n))

    def fromInput(): QuantumNumber = new QuantumNumber((1L to 9L).toSet)
  }

  val registers: mutable.HashMap[String, QuantumNumber] = mutable.HashMap(
      "w" -> QuantumNumber.fromNumber(0L),
      "x" -> QuantumNumber.fromNumber(0L),
      "y" -> QuantumNumber.fromNumber(0L),
      "z" -> QuantumNumber.fromNumber(0L),
  )

  implicit class Registers(map: mutable.HashMap[String, QuantumNumber]) {
    def safeGet(key: String): QuantumNumber =
      map.getOrElse(key, QuantumNumber.fromNumber(key.toLong))

    def dump(): Unit = {
      registers.foreach{ case (key, qn) =>
        println(s"  $key) size=${qn.set.size}, range=(${qn.set.min}, ${qn.set.max})")
      }
    }
  }

  val registerArray = Array.ofDim[Map[String, QuantumNumber]](operations.size)

  def process(): Unit = {
    var i = 0
    operations.foreach { operation =>
      println(s"$i. $operation")
      i += 1
      operation match {
        case ("inp", reg, _) =>
          registers(reg) = QuantumNumber.fromInput()
        case ("mul", reg, "0") =>
          registers(reg) = QuantumNumber.fromNumber(0)
        case ("mul", reg, "1") => ;
        case ("div", reg, "1") => ;
        case ("add", a, b) =>
          registers(a) = registers(a) add registers.safeGet(b)
        case ("mul", a, b) =>
          registers(a) = registers(a) mul registers.safeGet(b)
        case ("div", a, b) =>
          registers(a) = registers(a) div registers.safeGet(b)
        case ("mod", a, b) =>
          registers(a) = registers(a) mod registers.safeGet(b)
        case ("eql", a, b) =>
          registers(a) = registers(a) eql registers.safeGet(b)
      }
      registerArray(i-1) = registers.toMap
//      registers.dump()
    }
    println("Processed.")
  }

  Measure.dumpTime() {
    process()
  }
}
