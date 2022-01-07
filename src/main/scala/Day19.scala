import utils.EnumerationExtensions.RichSeq
import utils.ResourceFile

import scala.annotation.tailrec

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

  def edge(a: Point, b: Point): Set[Int] =
    Set(Math.abs(a._1 - b._1), Math.abs(a._2 - b._2), Math.abs(a._3 - b._3))

  type Edges = Map[Set[Int], Seq[Point]]

  def toEdges(block: Seq[Point]): Edges =
    block.combinations(2)
      .map { case List(a, b) => (edge(a, b), Seq(a, b)) }
      .toMap

  type BlockPairs = Seq[(Int, Int, Seq[Point], Seq[Point])]

  def blockPairs(): BlockPairs = {
    val edgeBlocks: Seq[Edges] =
      blocks.map(b => toEdges(b))

    for (
      Seq((edge1, i1), (edge2, i2)) <- edgeBlocks.zipWithIndex.combinations(2).toSeq;
      commonEdges = edge1.keySet & edge2.keySet if commonEdges.size > 65;
      commonPoints1 = commonEdges.flatMap(edge1(_)).toSeq;
      commonPoints2 = commonEdges.flatMap(edge2(_)).toSeq
    ) yield (i1, i2, commonPoints1, commonPoints2)
  }

  type TransformationMap = Map[Int, Seq[Transformation]]
  def buildTransformationMap(blockPairs: BlockPairs): TransformationMap = {

    @tailrec
    def collectTransformations(map: TransformationMap, newIndices: Set[Int]): TransformationMap = {
      val newTransformations = blockPairs.collect {
        case (i1, i2, c1, c2) if newIndices.contains(i1) => (i2, (i1, transformation(c1, c2)))
        case (i1, i2, c1, c2) if newIndices.contains(i2) => (i1, (i2, transformation(c2, c1)))
      }.toMap -- map.keySet
      val newMap = newTransformations
        .map { case (newIdx, (oldIdx, transformation)) => (newIdx, map(oldIdx) :+ transformation) }
      if (newMap.isEmpty)
        map
      else
        collectTransformations(map ++ newMap, newMap.keySet)
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
      val arguments = for (
        list1 <- dimensions1;
        (list2, index2) <- dimensions2.zipWithIndex;
        diff = Seq(list1, list2).transpose
          .map { case Seq(a, b) => a - b }
          .distinct if diff.length == 1
      ) yield (multipliers(index2), diff.head)

      point => {
        val (x, y, z) = point
        arguments match {
          case Seq((mx, dx), (my, dy), (mz, dz)) =>
            (
              mx._1 * x + mx._2 * y + mx._3 * z + dx,
              my._1 * x + my._2 * y + my._3 * z + dy,
              mz._1 * x + mz._2 * y + mz._3 * z + dz
            )
        }
      }
    }

    collectTransformations(Map(0 -> Seq.empty[Transformation]), Set(0))
  }

  lazy val transformationMap = buildTransformationMap(blockPairs())

  def beacons(): Set[Point] =
    transformationMap.flatMap { case (blockId, transformations) =>
      transformations.foldRight(blocks(blockId)) { (transformation, block) =>
        block.map(transformation)
      }
    }.toSet

  println(s"part 1: ${beacons().size}")

  def scanners(): Seq[Point] =
    transformationMap.map { case (_, transformations) =>
      transformations.foldRight((0, 0, 0)) { (transformation, scanner) =>
        transformation(scanner)
      }
    }.toSeq

  def manhattan(point1: Point, point2: Point): Int = {
    val (x1, y1, z1) = point1
    val (x2, y2, z2) = point2
    Math.abs(x1 - x2) + Math.abs(y1 - y2) + Math.abs(z1 - z2)
  }

  def maxManhattan(points: Seq[Point]): Int = {
    points.combinations(2)
      .map { case Seq(a, b) => manhattan(a, b) }
      .max
  }

  println(s"part 2: ${maxManhattan(scanners())}")
}
