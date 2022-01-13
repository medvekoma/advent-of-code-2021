import utils.{Measure, ResourceFile}

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._
import scala.util.Try

object Day24a extends App {

  case class Line(op: String, r1: String, r2: String)

  val iterator = (13 to 0 by -1).iterator
  val blocks = ResourceFile.readLines("day24.txt")
    .map(_.split(" "))
    .map(arr => Line(arr(0), arr(1), Try(arr(2)).getOrElse(iterator.next().toString)))
    .sliding(18, 18)
    .toList

  def binary(fn: (Long, Long) => Long,
             valid1: Long => Boolean = _ => true,
             valid2: Long => Boolean = _ => true)
            (x1: Long, x2: Long): Option[Long] = {
    if (valid1(x1) && valid2(x2))
      Some(fn(x1, x2))
    else
      None
  }

  def instruction(op: String): (Long, Long) => Option[Long] = op match {
    case "add" => binary(_ + _)
    case "mul" => binary(_ * _)
    case "div" => binary(_ / _, valid2 = _ != 0)
    case "mod" => binary(_ % _, valid1 = _ >= 0, valid2 = _ > 0)
    case "eql" => binary((a, b) => if (a == b) 1L else 0L)
  }

  def calculateZ(block: Seq[Line], w: Long, prevZ: Long): Option[Long] = {
    val registers = mutable.HashMap(
      "w" -> w,
      "z" -> prevZ,
      "x" -> 0L, "y" -> 0L
    )

    def getValue(reg: String): Long =
      registers.getOrElse(reg, reg.toLong)

    def evaluate(line: Line): Option[Long] =
      instruction(line.op)(getValue(line.r1), getValue(line.r2))

    block.tail.foreach { line =>
      evaluate(line) match {
        case None => return None
        case Some(value) =>
          registers(line.r1) = value
      }
    }
    registers.get("z")
  }

  def calculateBlock(block: Seq[Line], takeovers: Map[Long, (String, String)]): Map[Long, (String, String)] = {
    val res = for (
      w <- 1L to 9L;
      (prevZ, (min, max)) <- takeovers.par;
      z <- calculateZ(block, w, prevZ)
    ) yield (z, (min + w, max + w))
    res
      .groupMap(_._1)(_._2)
      .map { case (z, monads) => (z, (monads.map(_._1).min, monads.map(_._2).max)) }
      .seq
  }

  def calculateMonad(): (String, String) = {
    var takeovers = Map(0L -> ("", ""))
    for ((block, blockId) <- blocks.zipWithIndex) {
      println(s"$blockId / ${blocks.size}")
      if (blockId == 13)
        takeovers = takeovers.filter { case (key, _) => (13 to 21) contains key }
      takeovers = calculateBlock(block, takeovers)
    }
    takeovers(0)
  }

  val (min, max) = Measure.dumpTime() {
    calculateMonad()
  }
  println(s"part 1: $max")
  println(s"part 2: $min")
}
