import utils.ResourceFile

import scala.collection.mutable

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

  type IndexConstraints = List[(Int, Set[Int])]
  type ValueConstraints = List[(Int, IndexConstraints)]
  val registerValues = mutable.HashMap[Char, ValueConstraints](elems =
    'w' -> List((0, List.empty)),
    'x' -> List((0, List.empty)),
    'y' -> List((0, List.empty)),
    'z' -> List((0, List.empty))
  )
  val badDigits = mutable.HashMap[Int, Set[Int]]()

  operations.foreach { op =>
    op match {
      case Operation("inp", reg, _, Some(index)) =>
        registerValues(reg) = getInputValue(index)
      case Operation("add", reg, None, Some(n2)) =>
        registerValues(reg) = getValue1(reg, n2, _ + _)
      case Operation("add", reg1, Some(reg2), None) =>
        registerValues(reg1) = getValue2(reg1, reg2, _ + _)
      case Operation("mul", reg, None, Some(0)) =>
        registerValues(reg) = List(0 -> List.empty)
      case Operation("mul", reg, None, Some(n2)) =>
        registerValues(reg) = getValue1(reg, n2, _ * _)
      case Operation("mul", reg1, Some(reg2), None) =>
        registerValues(reg1) = getValue2(reg1, reg2, _ * _)
      case Operation("eql", reg, None, Some(n2)) =>
        registerValues(reg) = getValue1(reg, n2, (a, b) => if (a == b) 1 else 0)
      case Operation("eql", reg1, Some(reg2), None) =>
        registerValues(reg1) = getValue2(reg1, reg2, (a, b) => if (a == b) 1 else 0)
      case Operation("div", reg, None, Some(n2)) =>
        val newValues = registerValues(reg).toList
          .map { case (value, constraints) => (value / n2, constraints) }
        registerValues(reg) = unifyValues(newValues)
      case Operation("div", reg1, Some(reg2), None) =>
        val vc1 = registerValues(reg1)
        val vc2 = registerValues(reg2)
        val newValues = for (
          (v1, c1) <- vc1.toList;
          (v2, c2) <- vc2.toList;
          result = (v1 / v2, mergeConstraints(c1, c2)) if v2 != 0
        ) yield result
        registerValues(reg1) = unifyValues(newValues)
        for (
          (value, constraints) <- vc2 if value == 0;
          (index, set) <- constraints
        ) {
          badDigits(index) ++= set
        }
      case Operation("mod", reg, None, Some(n2)) =>
        val newValues = registerValues(reg).toList
          .map { case (value, constraints) => (value % n2, constraints) }
        registerValues(reg) = unifyValues(newValues)
      case Operation("mod", reg1, Some(reg2), None) =>
        val vc1 = registerValues(reg1)
        val vc2 = registerValues(reg2)
        val newValues = for (
          (v1, c1) <- vc1.toList if v1 >= 0;
          (v2, c2) <- vc2.toList if v2 > 0;
          result = (v1 % v2, mergeConstraints(c1, c2))
        ) yield result
        registerValues(reg1) = unifyValues(newValues)
        for (
          (v1, c1) <- vc1.toList if v1 < 0;
          (v2, c2) <- vc2.toList if v2 <= 0;
          (idx1, set1) <- c1;
          (idx2, set2) <- c2
        ) {
          badDigits(idx1) ++= set1
          badDigits(idx2) ++= set2
        }
      case operation =>
        println(s"ERROR: $operation")
    }

    println("---")
    println(op)
    registerValues.foreach(println)
    badDigits.foreach(println)
  }

  def getInputValue(index: Int): ValueConstraints =
    (1 to 9).map(digit => (digit, List((index, Set(digit))))).toList

  def getValue1(reg: Char, n2: Int, fn: (Int, Int) => Int): ValueConstraints = {
    val newValues = registerValues(reg)
      .map { case (value, constraints) => (fn(value, n2), constraints) }
    unifyValues(newValues)
  }

  def getValue2(reg1: Char, reg2: Char, fn: (Int, Int) => Int): ValueConstraints = {
    val vc1 = registerValues(reg1)
    val vc2 = registerValues(reg2)
    val newValues = for (
      (v1, c1) <- vc1;
      (v2, c2) <- vc2;
      result = (v1 * v2, mergeConstraints(c1, c2))
    ) yield result
    unifyValues(newValues)
  }

  def mergeConstraints(c1: IndexConstraints, c2: IndexConstraints): IndexConstraints =
    (c1 ++ c2)
      .groupMap(_._1)(_._2)
      .map { case (index, list) => (index, list.reduce(_ & _)) }
      .toList

  def unifyValues(vc: ValueConstraints): ValueConstraints = {
    val groupMap = vc.groupMap(_._1)(_._2)
    val result = groupMap
      .map { case (value, constraits) => (value, constraits.flatten
        .groupMap(_._1)(_._2).map { case (index, list) => (index, list.reduce(_ ++ _)) }.toSeq)
      }
    result.toSeq
  }
}
