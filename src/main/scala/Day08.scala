import scala.io.Source
import scala.util.Using

object Day08 extends App {

  type Line = (Seq[String], Seq[String])

  val lines = Using(Source.fromResource("day08.txt")) { source =>
    source.getLines()
      .map(_.split(" \\| "))
      .map { case Array(test, display) => (test.split(" ").toList, display.split(" ").toList) }
      .toList
  }.get

  def part1(lines: Seq[Line]): Int = {
    val validLengths = Seq(2, 4, 3, 7)
    lines
      .map { case (_, display) => display.count(digit => validLengths.contains(digit.length)) }
      .sum
  }

  def decode(test: Seq[String], display: Seq[String]): Int = {
    val testByLength = test
      .map(_.toCharArray.toSet)
      .groupBy(_.size)

    val d1 = testByLength(2).head
    val d4 = testByLength(4).head
    val d7 = testByLength(3).head
    val d8 = testByLength(7).head

//     aaaa
//    b    c
//    b    c
//     dddd
//    e    f
//    e    f
//     gggg

    val a = d7 -- d1
    val bd = d4 -- d1
    val adg = testByLength(5).reduce(_ & _)
    val abfg = testByLength(6).reduce(_ & _)
    val dg = adg -- a
    val b = bd -- dg
    val d = bd -- b
    val g = dg -- d
    val f = abfg -- a -- b -- g
    val c = d7 -- a -- f
    val e = d8 -- abfg -- c -- d

    val digitMap = Map (
      (d8 -- d) -> '0',
      d1 -> '1',
      (d8 -- b -- f) -> '2',
      (d8 -- b -- e) -> '3',
      d4 -> '4',
      (d8 -- c -- e) -> '5',
      (d8 -- c) -> '6',
      d7 -> '7',
      d8 -> '8',
      (d8 -- e) -> '9',
    )

    display
      .map(_.toCharArray.toSet)
      .map(digitMap.getOrElse(_, '*')) // let toInt fail if something goes wrong
      .mkString
      .toInt
  }

  def part2(lines: Seq[Line]): Int = {
    lines
      .map { case (test, display) => decode(test, display) }
      .sum
  }

  println(s"part 1: ${part1(lines)}")
  println(s"part 2: ${part2(lines)}")
}
