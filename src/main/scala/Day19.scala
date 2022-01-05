import utils.ListExtensions.RichList
import utils.ResourceFile

import scala.collection.mutable

object Day19 extends App {

  type Point = (Int, Int, Int)
  type Transformation = Point => Point

  val lines = ResourceFile.readLines("day19.txt")
  val blocks = lines
    .splitBySeparator("")
    .map { block =>
      block.tail.map { line =>
        line.split(',') match {
          case Array(x, y, z) => (x.toInt, y.toInt, z.toInt)
        }
      }
    }

  type PointDistances = Map[Set[Int], Seq[Point]]

  def distance(a: Point, b: Point): Set[Int] =
    Set(Math.abs(a._1 - b._1), Math.abs(a._2 - b._2), Math.abs(a._3 - b._3))

  def toDistance(block: Seq[Point]): PointDistances =
    block
      .combinations(2)
      .map { case List(a, b) => (distance(a, b), Seq(a, b)) }
      .toMap

  def blockPairs(): Seq[(Int, Int, Seq[Point], Seq[Point])] = {
    val pointDistances: Seq[PointDistances] =
      blocks.map(b => toDistance(b))

    for (
      i <- pointDistances.indices;
      j <- i + 1 until pointDistances.size;
      dist1 = pointDistances(i);
      dist2 = pointDistances(j);
      commonKeys = dist1.keySet & dist2.keySet if commonKeys.size > 65
    ) yield (i, j, commonKeys.flatMap(dist1(_)).toSeq, commonKeys.flatMap(dist2(_)).toSeq)
  }

  val pairs = blockPairs()
  val transformationMap = mutable.HashMap(0 -> Seq.empty[Transformation])

  def process(current: Int = 0): Unit = {
    val newBlocks = pairs.collect {
      case (i, j, c1, c2) if i == current => (j, transformation(c1, c2))
      case (i, j, c1, c2) if j == current => (i, transformation(c2, c1))
    }.toMap -- transformationMap.keySet
    newBlocks.foreach { case (id, transformation) =>
      transformationMap(id) = transformationMap(current) :+ transformation
      process(id)
    }
  }

  process()
  val newMap = transformationMap
    .map { case (blockId, transformations) =>
      (blockId, transformations.foldRight(blocks(blockId))((transformation, block) => block.map(transformation)))
    }

  val beacons: Set[Point] = newMap.values.flatten.toSet
  println(s"part 1: ${beacons.size}")

  val scanners = transformationMap.map {
    case (_, transformations) =>
      transformations.foldRight((0, 0, 0))((transformation, scanner) => transformation(scanner))
  }
  println(s"part 2: ${maxManhattan(scanners.toSeq)}")

  def manhattan(beacon1: Point, beacon2: Point): Int = {
    val (x1, y1, z1) = beacon1
    val (x2, y2, z2) = beacon2
    Math.abs(x1 - x2) + Math.abs(y1 - y2) + Math.abs(z1 - z2)
  }

  def maxManhattan(beacons: Seq[Point]): Int = {
    beacons.combinations(2)
      .map { case Seq(a, b) => manhattan(a, b) }
      .max
  }

  // Returns the transformation that maps block2 to the coordinates of block1
  def transformation(block1: Seq[Point], block2: Seq[Point]): Transformation = {
    val dimensions1 = block1
      .transpose { case (a, b, c) => Seq(a, b, c) }
      .map(list => list.sorted)
    val dimensions2a = block2
      .transpose { case (a, b, c) => Seq(a, b, c) }
      .map(list => list.sorted)
    val dimensions2b = dimensions2a
      .map(list => list.map(x => -x).reverse)
    val dimensions2 = dimensions2a ++ dimensions2b
    val multipliers = List(
      (+1, 0, 0), (0, +1, 0), (0, 0, +1),
      (-1, 0, 0), (0, -1, 0), (0, 0, -1)
    )
    val args = for (
      list1 <- dimensions1;
      (list2, index2) <- dimensions2.zipWithIndex;
      diff = Seq(list1, list2).transpose
        .map { case Seq(a, b) => a - b }
        .distinct if diff.length == 1
    ) yield (index2, diff.head)

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
