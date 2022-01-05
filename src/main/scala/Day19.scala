import utils.ListExtensions.RichList
import utils.ResourceFile

import scala.annotation.tailrec
import scala.collection.mutable

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

  def spinBlock(beacons: Set[Beacon]): Seq[Set[Beacon]] =
    beacons.map(spin).transpose.toSeq

  def toBeacon(string: String): Beacon =
    string.split(',') match {
      case Array(x, y, z) => (x.toInt, y.toInt, z.toInt)
    }

  val lines = ResourceFile.readLines("day19.txt")
  val blocks = lines
    .splitBySeparator("")
    .map(_.tail.map(toBeacon).toSet)

  implicit class RichBeacon(beacon: Beacon) {
    def +(other: Beacon): Beacon = (beacon._1 + other._1, beacon._2 + other._2, beacon._3 + other._3)

    def -(other: Beacon): Beacon = (beacon._1 - other._1, beacon._2 - other._2, beacon._3 - other._3)
  }

  def maxOverlap(block1: Set[Beacon], block2: Set[Beacon]): Option[Set[Beacon]] = {
    val differences = for (
      b1 <- block1.iterator;
      block2v <- spinBlock(block2);
      b2 <- block2v;
      diff = b1 - b2;
      block2m = block2v.map(_ + diff) if (block1 & block2m).size >= 12
    ) yield block2m
    differences.nextOption()
  }

  var unified = blocks.head

  @tailrec
  def findOverlaps(blocks: Seq[Set[Beacon]]): Unit = {
    println(s"... ${blocks.size}")
    if (blocks.nonEmpty) {
      val (overlaps, noOverlaps) = blocks
        .map { block => (block, maxOverlap(unified, block)) }
        .partition(_._2.isDefined)
      overlaps.foreach(unified ++= _._2.get)
      findOverlaps(noOverlaps.map(_._1))
    }
  }

  val indexedBlocks = blocks.zipWithIndex
    .map{ case (block, index) => (index, block)}
    .toMap

  val unifiedBlocks = mutable.HashMap[Int, Set[Beacon]]()

  def unify(blocks: Map[Int, Set[Beacon]], key: Int): Unit = {
    if (blocks.nonEmpty) {
      val homeBlock = unifiedBlocks(key)
      unifiedBlocks += (key -> homeBlock)
      val foundBlocks = (blocks - key)
        .map { case (idx, block) => (idx, maxOverlap(homeBlock, block)) }
        .collect { case (idx, Some(transformedBlock)) => (idx, transformedBlock) }
      unifiedBlocks ++= foundBlocks
      foundBlocks.foreach {
        case (idx, _) =>
          val remainingMap = indexedBlocks -- unifiedBlocks.keySet
          println(remainingMap.size)
          unify(remainingMap, idx)
      }
    }
  }

  do {
    val remainingBlocks = indexedBlocks -- unifiedBlocks.keySet
    val key = remainingBlocks.head._1
    unifiedBlocks += remainingBlocks.head
    unify(remainingBlocks, key)
  } while (indexedBlocks.size > unifiedBlocks.size)

  unify(indexedBlocks, 0)
  println(indexedBlocks.size, unifiedBlocks.size)
  println(unifiedBlocks.values.reduce(_ ++ _).size)


//  findOverlaps(blocks.tail)
//  println(s"part 1: ${res.size}")
}
