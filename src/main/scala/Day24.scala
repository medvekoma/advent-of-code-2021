import utils.{Measure, ResourceFile}
import scala.util.Try

object Day24 extends App {

  val iterator = (13 to 0 by -1).iterator

  case class Operation(op: String, r1: String, r2: String)

  val operationBlocks = ResourceFile.readLines("day24.txt")
    .map(_.split(" "))
    .map(arr => Operation(arr(0), arr(1), Try(arr(2)).getOrElse(iterator.next().toString)))
    .sliding(18, 18)
    .toList

  case class Registers(w: Long, z: Long, x: Long = 0L, y: Long = 0L) {

    def get(reg: String): Long = reg match {
      case "w" => w
      case "x" => x
      case "y" => y
      case "z" => z
      case v => v.toLong
    }

    def set(reg: String, value: Long): Registers = reg match {
      case "w" => this.copy(w = value)
      case "x" => this.copy(x = value)
      case "y" => this.copy(y = value)
      case "z" => this.copy(z = value)
    }
  }

  def binary(fn: (Long, Long) => Long,
             valid1: Long => Boolean = _ => true,
             valid2: Long => Boolean = _ => true
            )(
              x1: Long, x2: Long,
            ): Option[Long] = {
    if (valid1(x1) && valid2(x2))
      Some(fn(x1, x2))
    else
      None
  }

  def operation(op: String) = op match {
    case "add" => binary(_ + _) _
    case "mul" => binary(_ * _) _
    case "div" => binary(_ / _, valid2 = _ != 0) _
    case "mod" => binary(_ % _, valid1 = _ >= 0, valid2 = _ > 0) _
    case "eql" => binary((a, b) => if (a == b) 1L else 0L) _
  }

  def calculateZ(operations: Seq[Operation], w: Long, prevZ: Long): Option[Long] = {
    val lastRegisters = operations.tail.foldLeft(Registers(w, prevZ)) { (regs, line) =>
      operation(line.op)(regs.get(line.r1), regs.get(line.r2)) match {
        case None => return None
        case Some(value) =>
          regs.set(line.r1, value)
      }
    }
    Some(lastRegisters.z)
  }

  def calculateBlock(block: Int, prevZs: Map[Long, (String, String)]): Map[Long, (String, String)] = {
    val res = for (
      w <- 1L to 9L;
      (prevZ, (monadMin, monadMax)) <- prevZs;
      nextZ <- calculateZ(operationBlocks(block), w, prevZ)
    ) yield (nextZ, (monadMin + w, monadMax + w))
    res
      .groupMap(_._1)(_._2)
      .map { case (nextZ, pair) => (nextZ, (pair.map(_._1).min, pair.map(_._2).max)) }
  }

  def calculateMonadMinMax(): (String, String) = {
    var prevZs = Map(0L -> ("", ""))
    for (block <- operationBlocks.indices) {
      println(block)
      prevZs = calculateBlock(block, prevZs)
    }
    prevZs(0)
  }

  val (min, max) = Measure.dumpTime() {
    calculateMonadMinMax()
  }
  println(s"part 1: $max")
  println(s"part 2: $min")
}
