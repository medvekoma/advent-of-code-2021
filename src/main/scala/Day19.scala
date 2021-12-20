import utils.ListExtensions.RichList
import utils.ResourceFile

object Day19 extends App {

  type Beacon = (Int, Int, Int)

  def spin3(beacon: Beacon): Seq[Beacon] = {
    val (a, b, c) = beacon
    Seq((a, b, c), (b, c, a), (c, a, b))
  }

  def spin(beacon: Beacon): Seq[Beacon] = {
    val (x, y, z) = beacon
    val octave = Seq(
      (x, y, z),
      (y, -x, z),
      (-x, -y, z),
      (-y, x, z),
      (x, -y, -z),
      (-y, -x, -z),
      (-x, y, -z),
      (y, z, -z)
    )
    octave.flatMap(spin3)
  }

  def spinMatrix(beacons: Seq[Beacon]): Seq[Seq[Beacon]] =
    beacons.map(spin)

  def toBeacon(string: String): Beacon =
    string.split(',') match {
      case Array(x, y, z) => (x.toInt, y.toInt, z.toInt)
    }

  val lines = ResourceFile.readLines("_day19.txt")
  val blocks = lines
    .splitBySeparator("")
    .map(_.tail.map(toBeacon))
  blocks.foreach(block => println(block.size, block))

  implicit class RichBeacon(beacon: Beacon) {
    def +(other: Beacon): Beacon = (beacon._1 + other._1, beacon._2 + other._2, beacon._3 + other._3)

    def -(other: Beacon): Beacon = (beacon._1 - other._1, beacon._2 - other._2, beacon._3 - other._3)
  }

  implicit class RichBeacons(beacons: Seq[Beacon]) {
    def -(other: Seq[Beacon]): Seq[Beacon] =
      beacons
        .zip(other)
        .map { case (b1, b2) => b1 - b2 }
  }

  val matrix1 = blocks.head
  val matrix2 = blocks.tail.head

  matrix2.map(spin)
    .map(matrix => matrix1 - matrix)
    .foreach(println)
}
