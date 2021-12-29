//import utils.ResourceFile
//
//import scala.collection.mutable
//
//object Day24 extends App {
//
//  val iterator = (0 to 13).iterator
//  val inpPattern = "inp ([wxyz])".r
//  val expressionPattern = "(\\w+) ([wxyz]) ([wxyz]?)(-?\\d*)".r
//
//  case class Operation(op: String, reg: Char, reg2: Option[Char], value: Option[Int])
//
//  val operations = ResourceFile.readLines("day24.txt")
//    .collect {
//      case inpPattern(reg) => Operation("inp", reg.head, None, Some(iterator.next()))
//      case expressionPattern(op, reg, reg2, value) => Operation(op, reg.head, reg2.headOption, value.toIntOption)
//    }
//
////  sealed trait Constraint
////  case object Always extends Constraint
////  case object Never extends Constraint
////  case class Input(index: Int, values: Set[Int]) extends Constraint
////  case class And(constraints: Constraint*) extends Constraint
////  case class Or(constraints: Constraint*) extends Constraint
////  object Constraint {
////    def and(constraints: List[Constraint]): Constraint = {
////      val list = constraints.filterNot(_ == Always)
////      list match {
////        case List() => Never
////        case List(constraint) => constraint
////        case inputs: List[Input] =>
////          inputs.groupMap(_.index)(_.values)
////            .map { case (index, sets) => Input(index, sets.reduce(_ & _))}
////            .filter(_.values.nonEmpty)
////        case _ => And(list:_*)
////      }
////    }
////
////    def or(constraints: List[Constraint]): Constraint =
////      if (constraints.contains(Always))
////        Always
////      else constraints match {
////        case List() => Never
////        case List(constraint) => constraint
////        case _ => Or(constraints:_*)
////      }
////  }
////
//  class QuantumNumber(val value: Map[Long, Constraint]) {
//    override def toString: String =
//      s"QuantumNumber(${value.mkString("\n  ", "\n  ", "\n")})"
//
//    def add(that: QuantumNumber): QuantumNumber =
//      operation(that, _ + _)
//
//    def mul(that: QuantumNumber): QuantumNumber =
//      operation(that, _ * _)
//
//    def div(that: QuantumNumber): QuantumNumber =
//      operation(that, _ / _, valid2 = _ != 0)
//
//    def mod(that: QuantumNumber): QuantumNumber =
//      operation(that, _ % _, valid1 = _ >= 0, valid2 = _ > 0)
//
//    def eql(that: QuantumNumber): QuantumNumber =
//      operation(that, (a, b) => if (a == b) 1 else 0)
//
//    def operation(op: String): QuantumNumber => QuantumNumber =
//      op match {
//        case "add" => add
//        case "mul" => mul
//        case "div" => div
//        case "mod" => mod
//        case "eql" => eql
//      }
//
//    private def operation(that: QuantumNumber, fn: (Long, Long) => Long,
//                          valid1: Long => Boolean = _ => true,
//                          valid2: Long => Boolean = _ => true
//                         ): QuantumNumber = {
//      val values = for (
//        (v1, c1) <- this.value.toList;
//        (v2, c2) <- that.value.toList
//      ) yield (v1, v2, c1, c2)
//      val value = values.collect {
//        case (v1, v2, c1, c2) if valid1(v1) && valid2(v2) =>
//          (fn(v1, v2), Constraint.and(List(c1, c2)))
//      }.groupMap(_._1)(_._2)
//        .map { case (value, constraints) => (value, Constraint.or(constraints)) }
//      new QuantumNumber(value)
//    }
//  }
//
//  object QuantumNumber {
//    def fromValue(value: Long): QuantumNumber =
//      new QuantumNumber(Map(value -> Always))
//
//    def fromInput(index: Int): QuantumNumber = {
//      val value = (1 to 9)
//        .map(i => (i.toLong, Input(index, Set(i))))
//        .toMap
//      new QuantumNumber(value)
//    }
//  }
//
//  val v0 = QuantumNumber.fromInput(0)
//  val v1 = QuantumNumber.fromInput(1)
//  println (v0)
//  println (v1)
//  println (v0 eql v1)
//
//  def process() {
//    val registers = mutable.HashMap(
//      'w' -> QuantumNumber.fromValue(0),
//      'x' -> QuantumNumber.fromValue(0),
//      'y' -> QuantumNumber.fromValue(0),
//      'z' -> QuantumNumber.fromValue(0),
//    )
//
//    var i = 0
//    operations.foreach { operation =>
//      println(s"$i. $operation")
//      i += 1
//      operation match {
//        case Operation("inp", reg, None, Some(index)) =>
//          registers(reg) = QuantumNumber.fromInput(index)
//        case Operation("mul", reg, None, Some(0)) =>
//          registers(reg) = QuantumNumber.fromValue(0)
//        case Operation("mul", reg, None, Some(1)) =>
//        case Operation("div", reg, None, Some(1)) =>
//          ;
//        case Operation(op, reg, None, Some(value)) =>
//          registers(reg) = registers(reg).operation(op)(QuantumNumber.fromValue(value))
//        case Operation(op, reg1, Some(reg2), None) =>
//          registers(reg1) = registers(reg1).operation(op)(registers(reg2))
//      }
//      registers.foreach(println)
//    }
//    println(registers('z'))
//  }
//
//  process()
//}
