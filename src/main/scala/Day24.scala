import utils.{Measure, ResourceFile}

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._
import scala.util.Try

object Day24 extends App {

  val iterator = (13 to 0 by -1).iterator

  case class Line(op: String, r1: String, r2: String)

  val blocks = ResourceFile.readLines("day24.txt")
    .map(_.split(" "))
    .map(arr => Line(arr(0), arr(1), Try(arr(2)).getOrElse(iterator.next().toString)))
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

  def operation(fn: (Long, Long) => Long, valid1: Long => Boolean = _ => true, valid2: Long => Boolean = _ => true)
               (x1: Long, x2: Long): Option[Long] = {
    if (valid1(x1) && valid2(x2))
      Some(fn(x1, x2))
    else
      None
  }

  def binary(op: String) = op match {
    case "add" => operation(_ + _) _
    case "mul" => operation(_ * _) _
    case "div" => operation(_ / _, valid2 = _ != 0) _
    case "mod" => operation(_ % _, valid1 = _ >= 0, valid2 = _ > 0) _
    case "eql" => operation((a, b) => if (a == b) 1L else 0L) _
  }

  def calculateZ(operations: Seq[Line], w: Long, prevZ: Long): Option[Long] = {
    val registers = mutable.HashMap(
      "w" -> w,
      "z" -> prevZ,
      "x" -> 0L, "y" -> 0L
    )
    def getValue(reg: String) = registers.getOrElse(reg, reg.toLong)

    operations.tail.foreach { line =>
      binary(line.op)(getValue(line.r1), getValue(line.r2)) match {
        case None => return None
        case Some(value) =>
          registers(line.r1) = value
      }
    }
    registers.get("z")
  }

  case class Monad(min: String, max: String)

  def calculateBlock(block: Int, takeovers: Map[Long, Monad]): Map[Long, Monad] = {
    val res = for (
      w <- 1L to 9L;
      (prevZ, monad) <- takeovers.par;
      z <- calculateZ(blocks(block), w, prevZ)
    ) yield (z, Monad(monad.min + w, monad.max + w))
    res
      .groupMap(_._1)(_._2)
      .map { case (z, monads) => (z, Monad(monads.map(_.min).min, monads.map(_.max).max)) }
  }

  def calculateMonad(): Monad = {
    var takeovers = Map(0L -> Monad("", ""))
    for (block <- blocks.indices) {
      println(block)
      takeovers = calculateBlock(block, takeovers)
    }
    takeovers(0)
  }

  val monad = Measure.dumpTime() {
    calculateMonad()
  }
  println(s"part 1: ${monad.max}")
  println(s"part 2: ${monad.min}")
}
