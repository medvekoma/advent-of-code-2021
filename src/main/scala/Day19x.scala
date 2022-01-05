import utils.ListExtensions.RichList
import utils.ResourceFile

object Day19x extends App {

  type Beacon = (Int, Int, Int)

  val lines = ResourceFile.readLines("_day19.txt")
  val blocks = lines
    .splitBySeparator("")
    .map(_.tail.map(_.split(',') match {
      case Array(x, y, z) => (x.toInt, y.toInt, z.toInt)
    }))

  type AbsDistance = Map[Set[Int], Seq[Beacon]]

  def toAbsDistance(block: Seq[Beacon]): AbsDistance = {
    val all = for (
      i <- block.indices;
      j <- i + 1 until block.size;
      a = block(i);
      b = block(j);
      diff = Set(Math.abs(a._1 - b._1), Math.abs(a._2 - b._2), Math.abs(a._3 - b._3))
    ) yield (diff, Seq(a, b))
    all.toMap // .sorted.take(20 * block.size)
  }

  val absDistances: Seq[AbsDistance] =
    blocks.map(b => toAbsDistance(b))

  def common(id1: Int, id2: Int): Set[Beacon] = {
    val d1 = absDistances(id1)
    val d2 = absDistances(id2)
    val commonKeys = d1.keySet & d2.keySet
    if (commonKeys.size > 65)
      commonKeys.flatMap(d1(_)) ++ commonKeys.flatMap(d2(_))
    else
      Set.empty
  }

  def blockPairs(): Seq[(Int, Int)] =
    for (
      i <- absDistances.indices;
      j <- i + 1 until absDistances.size;
      commons = common(i, j) if commons.nonEmpty
    ) yield (i, j)

  implicit class RichBeacon(beacon: Beacon) {
    def +(other: Beacon): Beacon = (beacon._1 + other._1, beacon._2 + other._2, beacon._3 + other._3)

    def -(other: Beacon): Beacon = (beacon._1 - other._1, beacon._2 - other._2, beacon._3 - other._3)
  }

  //  blockPairs().foreach(println)

  type Rotation = Beacon => Beacon

  val spin3: Iterable[Rotation] =
    Seq(
      beacon => beacon,
      b => (b._2, b._3, b._1),
      b => (b._3, b._1, b._2)
    )

  val spin8: Iterable[Rotation] =
    Seq(
      beacon => beacon,
      b => (+b._2, -b._1, +b._3),
      b => (-b._1, -b._2, +b._3),
      b => (-b._2, +b._1, +b._3),
      b => (+b._1, -b._2, -b._3),
      b => (-b._2, -b._1, -b._3),
      b => (-b._1, +b._2, -b._3),
      b => (+b._2, +b._1, -b._3)
    )

  val rotations: Iterable[Rotation] = {
    for (
      s3 <- spin3;
      s8 <- spin8;
      s = s3 andThen s8
    ) yield s
  }

  // 429 too low
}
