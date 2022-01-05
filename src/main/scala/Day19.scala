import utils.ListExtensions.RichList
import utils.ResourceFile

import scala.collection.mutable

object Day19 extends App {

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
    Set(next) ++ links
      .map(link => collect(collected + next, blockPairs, link))
      .foldLeft(Set.empty[Int])(_ ++ _)
  }

  val pairs = blockPairs()
  val transformationMap = mutable.HashMap(0 -> Seq.empty[Transformation])
  val verificationMap = mutable.HashMap(0 -> Seq.empty[(Int, Int)])

  def process(current: Int = 0): Unit = {
    val newBlocks = pairs.collect {
      case (i, j, c1, c2) if i == current => (j, transformation(c1, c2))
      case (i, j, c1, c2) if j == current => (i, transformation(c2, c1))
    }.toMap -- transformationMap.keySet
    newBlocks.foreach { case (id, transformation) =>
      verificationMap(id) = verificationMap(current) :+ (current -> id)
      transformationMap(id) = transformationMap(current) :+ transformation
      process(id)
    }
  }

  process()
  val newMap = transformationMap
    .map { case (blockId, transformations) =>
      (blockId, transformations.foldRight(blocks(blockId))((transformation, block) => block.map(transformation)))
    }

  val beacons: Set[(Int, Int, Int)] = newMap.values.flatten.toSet
  println(s"part 1: ${beacons.size}")

  val scanners = transformationMap.map {
    case (_, transformations) =>
      transformations.foldRight((0,0,0))((transformation, scanner) => transformation(scanner))
  }
  println(s"part 2: ${maxManhattan(scanners.toSeq)}")

  def manhattan(beacon1: Beacon, beacon2: Beacon): Int = {
    val (x1, y1, z1) = beacon1
    val (x2, y2, z2) = beacon2
    Math.abs(x1 - x2) + Math.abs(y1 - y2) + Math.abs(z1 - z2)
  }

  def maxManhattan(beacons: Seq[Beacon]): Int = {
    beacons.combinations(2)
      .map { case Seq(a, b) => manhattan(a, b) }
      .max
  }

  def boundingBox(beacons: Seq[Beacon]): Seq[Int] = {
    beacons.transpose { case (a, b, c) => Seq(a, b, c) } match {
      case Seq(xs, ys, zs) => Seq(xs.max - xs.min, ys.max - ys.min, zs.max - zs.min)
    }
  }

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
