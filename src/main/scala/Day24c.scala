import utils.ResourceFile

import scala.collection.{Set, mutable}

object Day24c extends App {

  val iterator = (13 to 0 by -1).iterator
  val inpPattern = "inp ([wxyz])".r
  val expressionPattern = "(\\w+) ([wxyz]) ([wxyz]?)(-?\\d*)".r

  case class Operation(op: String, reg: Char, reg2: Option[Char], value: Option[Int])

  val operations = ResourceFile.readLines("day24.txt")
    .collect {
      case inpPattern(reg) => Operation("inp", reg.head, None, Some(iterator.next()))
      case expressionPattern(op, reg, reg2, value) => Operation(op, reg.head, reg2.headOption, value.toIntOption)
    }

  sealed trait Node {
    val values: Map[Long, Set[(Long, Long)]]

    def operation(left: Node, right: Node,
                  fn: (Long, Long) => Long,
                  valid1: Long => Boolean = _ => true,
                  valid2: Long => Boolean = _ => true
                 ): Map[Long, Set[(Long, Long)]] = {
      val internalValues = for (
        (v1, _) <- left.values.toSeq;
        (v2, _) <- right.values.toSeq;
        v = (fn(v1, v2), (v1, v2)) if valid1(v1) && valid2(v2)
      ) yield v

      internalValues.groupMap(_._1)(_._2)
        .map { case (value, list) => (value, list.toSet) }

    }
  }

  object Node {
    def binary(op: String, left: Node, right: Node): Node =
      op match {
        case "add" => Add(left, right)
        case "mul" => Mul(left, right)
        case "div" => Div(left, right)
        case "mod" => Mod(left, right)
        case "eql" => Eql(left, right)
      }
  }

  case class Number(value: Long) extends Node {
    override val values: Map[Long, Set[(Long, Long)]] = Map(value -> Set.empty)
  }

  case class Input(index: Int) extends Node {
    override val values: Map[Long, Set[(Long, Long)]] =
      (1 to 9).map(i => i.toLong -> Set[(Long, Long)]()).toMap
  }

  sealed trait Binary extends Node {
    val left: Node
    val right: Node
  }

  case class Add(left: Node, right: Node) extends Binary {
    override val values: Map[Long, Set[(Long, Long)]] = operation(left, right, _ + _)
  }

  case class Mul(left: Node, right: Node) extends Binary {
    override val values: Map[Long, Set[(Long, Long)]] = operation(left, right, _ * _)
  }

  case class Div(left: Node, right: Node) extends Binary {
    override val values: Map[Long, Set[(Long, Long)]] = operation(left, right, _ / _, valid2 = _ != 0)
  }

  case class Mod(left: Node, right: Node) extends Binary {
    override val values: Map[Long, Set[(Long, Long)]] = operation(left, right, _ % _, valid1 = _ >= 0, valid2 = _ > 0)
  }

  case class Eql(left: Node, right: Node) extends Binary {
    override val values: Map[Long, Set[(Long, Long)]] = operation(left, right, (a, b) => if (a == b) 1L else 0L)
  }

  val registers: mutable.HashMap[Char, Node] = mutable.HashMap(
    'w' -> Number(0L),
    'x' -> Number(0L),
    'y' -> Number(0L),
    'z' -> Number(0L),
  )

  def process() {
    var i = 0
    operations.foreach { operation =>
      println(s"$i. $operation")
      i += 1
      operation match {
        case Operation("inp", reg, None, Some(index)) =>
          registers(reg) = Input(index)
        case Operation("mul", reg, None, Some(0)) =>
          registers(reg) = Number(0)
        case Operation("mul", reg, None, Some(1)) =>
        case Operation("div", reg, None, Some(1)) =>
          ;
        case Operation(op, reg, None, Some(value)) =>
          registers(reg) = Node.binary(op, registers(reg), Number(value))
        case Operation(op, reg1, Some(reg2), None) =>
          registers(reg1) = Node.binary(op, registers(reg1), registers(reg2))
      }
    }
    println("Processed.")
    val res = collectMax(registers('z'), 0L)
    println(asString(res))
//    val inputs = collectInputs(registers('z'), 0L)
//    println(s"Inputs size: ${inputs.size}")
//    val maxString = inputs
//      .map(asString)
//      .max
//    println(s"Max: $maxString")
  }

  object InputOrdering extends Ordering[Map[Int, Int]] {
    override def compare(x: Map[Int, Int], y: Map[Int, Int]): Int =
      asString(x) compare asString(y)
  }

  def asString(inputMap: Map[Int, Int]): String =
    (13 to 0 by -1)
      .map(inputMap.get(_).map(_.toString.head).getOrElse('*'))
      .mkString

  def joinMax(map1: Map[Int, Int], map2: Map[Int, Int]): Map[Int, Int] = {
    val value = (map1.toList ++ map2).groupMap(_._1)(_._2)
    if (value.exists(_._2.toSet.size > 1))
      println("ERROR:" + value)
    value.map{ case (key, list) => (key, list.head)}

  }

  def joinInputs(list1: List[Map[Int, Int]], list2: List[Map[Int, Int]]): List[Map[Int, Int]] = {
    for (
      map1 <- list1;
      map2 <- list2;
//      input = println(s"Input: ${asString(map1)} + ${asString(map2)}");
      value = (map1.toList ++ map2).groupMap(_._1)(_._2) if !value.exists(_._2.toSet.size > 1);
      res = value.map{ case (key, list) => (key, list.head)}
//      output = println(s"  res: ${asString(res)}")
    ) yield res
  }

  def collectInputs(node: Node, value: Long): List[Map[Int, Int]] =
    node match {
      case Number(value) => List(Map.empty)
      case Input(index) => List(Map(index -> value.toInt))
      case binary: Binary =>
        node.values(value).toList
          .flatMap { case (leftVal, rightVal) => joinInputs(collectInputs(binary.left, leftVal), collectInputs(binary.right, rightVal)) }
    }

  def collectMax(node: Node, value: Long): Map[Int, Int] =
    node match {
      case Number(value) => Map.empty
      case Input(index) => Map(index -> value.toInt)
      case binary: Binary =>
        node.values(value).toList
          .map { case (leftVal, rightVal) => joinMax(collectMax(binary.left, leftVal), collectMax(binary.right, rightVal)) }
          .maxBy(asString)
    }

  def validate(code: String) {
    val registers = mutable.HashMap(
      'z' -> 0L,
      'x' -> 0L,
      'y' -> 0L,
      'z' -> 0L,
    )
    var i = 0
    operations.foreach { operation =>
      println(s"$i. $operation")
      i += 1
      operation match {
        case Operation("inp", reg, None, Some(index)) =>
          registers(reg) = code(index).asDigit.toLong
        case Operation("mul", reg, None, Some(0)) =>
          registers(reg) = 0L
        case Operation("mul", reg, None, Some(1)) =>
        case Operation("div", reg, None, Some(1)) =>
          ;
        case Operation(op, reg, None, Some(value)) =>
          registers(reg) = Node.binary(op, Number(registers(reg)), Number(value)).values.head._1
        case Operation(op, reg1, Some(reg2), None) =>
          registers(reg1) = Node.binary(op, Number(registers(reg1)), Number(registers(reg2))).values.head._1
      }
    }
    println(registers('z'))
  }


  process()
//  println(joinInputs(List(Map(0 -> 2, 1-> 3)), List(Map(1 -> 2))))
//  validate("11118411111314")
  // 11118411111314
  //  println(asString(Map(1 -> 9, 13 -> 7)))
}
