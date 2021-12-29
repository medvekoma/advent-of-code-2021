import utils.ResourceFile

import scala.collection.mutable

object Day24c extends App {

  val iterator = (0 to 13).iterator
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
        (v1, _) <- left.values;
        (v2, _) <- right.values;
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
    val result = collectInputs(registers('z'), 0L)
      .groupMap(_._1)(_._2)
      .map { case (index, set) => (index, set.toList.sorted )}
      .toList.sortBy(_._1)
    result.foreach(println)
  }

  def collectInputs(node: Node, value: Long): Set[(Int, Int)] = {
    node match {
      case Input(index) => Set((index, value.toInt))
      case Number(_) => Set.empty
      case binary: Binary =>
        node.values(value)
          .map { case (left, right) => collectInputs(binary.left, left) ++ collectInputs(binary.right, right) }
          .reduce(_ ++ _)
    }
  }

  process()
}
