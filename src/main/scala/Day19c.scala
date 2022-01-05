import utils.ListExtensions.RichList
import utils.ResourceFile

object Day19c extends App {

  type Beacon = (Int, Int, Int)
  type Transformation = Beacon => Beacon

  def toBeacon(string: String): Beacon =
    string.split(',') match {
      case Array(x, y, z) => (x.toInt, y.toInt, z.toInt)
    }

  val lines = ResourceFile.readLines("day19.txt")
  val blocks = lines
    .splitBySeparator("")
    .map(_.tail.map(toBeacon))

  implicit class RichBeacon(beacon: Beacon) {
    def +(other: Beacon): Beacon = (beacon._1 + other._1, beacon._2 + other._2, beacon._3 + other._3)

    def -(other: Beacon): Beacon = (beacon._1 - other._1, beacon._2 - other._2, beacon._3 - other._3)
  }

  type BlockDistance = Map[Set[Int], Seq[Beacon]]

  def toDistance(block: Seq[Beacon]): BlockDistance = {
    val all = for (
      i <- block.indices;
      j <- i + 1 until block.size;
      a = block(i);
      b = block(j);
      diff = Set(Math.abs(a._1 - b._1), Math.abs(a._2 - b._2), Math.abs(a._3 - b._3))
    ) yield (diff, Seq(a, b))
    all.toMap // .sorted.take(20 * block.size)
  }

  val distances: Seq[BlockDistance] =
    blocks.map(b => toDistance(b))

  def common(d1: BlockDistance, d2: BlockDistance): (Seq[Beacon], Seq[Beacon]) = {
    val commonKeys = (d1.keySet & d2.keySet).toSeq
    if (commonKeys.size > 65)
      (commonKeys.flatMap(d1(_)), commonKeys.flatMap(d2(_)))
    else
      (Seq.empty, Seq.empty)
  }

  def blockPairs(): Seq[(Int, Int, Seq[Beacon], Seq[Beacon])] =
    for (
      i <- distances.indices;
      j <- i + 1 until distances.size;
      (common1, common2) = common(distances(i), distances(j)) if common1.nonEmpty
    ) yield (i, j, common1, common2)

  def collect(collected: Set[Int], blockPairs: Seq[(Int, Int, Seq[Beacon], Seq[Beacon])], next: Int): Set[Int] = {
    val links = blockPairs.collect {
      case (i, j, c1, c2) if i == next => j
      case (i, j, c1, c2) if j == next => i
    }.toSet -- collected
    println(next, links)
    Set(next) ++ links
      .map(link => collect(collected + next, blockPairs, link))
      .foldLeft(Set.empty[Int])(_ ++ _)
  }

  println(collect(Set.empty, blockPairs(), 0).size)

  def transformation(block1: Seq[Beacon], block2: Seq[Beacon]): Transformation = {
    val indices1 = block1.transpose { case (a, b, c) => Seq(a, b, c) }
      .map(list => list.sorted)
    val indices2a = block2.transpose { case (a, b, c) => Seq(a, b, c) }
      .map(list => list.sorted)
    val indices2b = indices2a
      .map(list => list.map(x => -x).reverse)
    val indices2 = indices2a ++ indices2b
    val args = for (
      (list1, index1) <- indices1.zipWithIndex;
      (list2, index2) <- indices2.zipWithIndex;
      diff = Seq(list1, list2).transpose
        .map { case Seq(a, b) => a - b }
        .distinct if diff.length == 1
    ) yield (index2, diff.head)
    val multipliers = Map(
      0 -> (1, 0, 0),
      1 -> (0, 1, 0),
      2 -> (0, 0, 1),
      3 -> (-1, 0, 0),
      4 -> (0, -1, 0),
      5 -> (0, 0, -1)
    )

    beacon => {
      val (x, y, z) = beacon
      args match {
        case Seq((mx, sx), (my, sy), (mz, sz)) =>
          val (ax, bx, cx) = multipliers(mx)
          val (ay, by, cy) = multipliers(my)
          val (az, bz, cz) = multipliers(mz)
          (
            ax * x + bx * y + cx * z + sx,
            ay * x + by * y + cy * z + sy,
            az * x + bz * y + cz * z + sz
          )
      }
    }
  }
}
