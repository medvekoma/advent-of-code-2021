import utils.ResourceFile
import utils.RangeExtensions._

object Day22 extends App {

  val lines = ResourceFile.readLines("day22.txt")
  val pattern = "(\\w+) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)".r

  case class Operation(mode: Boolean, x: Seq[Int], y: Seq[Int], z: Seq[Int])

  val operations = lines.collect { case pattern(mode, x1, x2, y1, y2, z1, z2) =>
    Operation(mode == "on", x1.toInt to x2.toInt, y1.toInt to y2.toInt, z1.toInt to z2.toInt)
  }

  val validRange = -50 to 50
  var cubesOn = Set[(Int, Int, Int)]()
  operations.foreach { operation =>
    val xRange = operation.x intersect validRange
    val yRange = operation.y intersect validRange
    val zRange = operation.z intersect validRange
    val cubes = for (x <- xRange; y <- yRange; z <- zRange) yield (x, y, z)
    if (operation.mode)
      cubesOn ++= cubes.toSet
    else
      cubesOn --= cubes.toSet
  }

  println(s"part 1: ${cubesOn.size}")

  case class Cube(xRange: Seq[Int], yRange: Seq[Int], zRange: Seq[Int]) {
    def nonEmpty: Boolean = xRange.nonEmpty && yRange.nonEmpty && zRange.nonEmpty

    def subtract(that: Cube): Seq[Cube] =
      (
        (this.xRange subtract that.xRange).map(xRange => Cube(xRange, this.yRange, this.zRange)) ++
          (this.yRange subtract that.yRange).map(yRange => Cube(this.xRange & that.xRange, yRange, this.zRange)) ++
          (this.zRange subtract that.zRange).map(zRange => Cube(this.xRange & that.xRange, this.yRange & that.yRange, zRange))
        ).filter(x => x.nonEmpty)

    def intersects(that: Cube): Boolean =
      (this.xRange intersects that.xRange) &&
        (this.yRange intersects that.yRange) &&
        (this.zRange intersects that.zRange)

    def size: Long =
      xRange.size.toLong * yRange.size * zRange.size

    def containsAll(that: Cube): Boolean =
      this.xRange.containsAll(that.xRange) && this.yRange.containsAll(that.yRange) && this.zRange.containsAll(that.zRange)
  }


  var i = 0
  val result = operations
    .map { case Operation(mode, xRange, yRange, zRange) => (mode, Cube(xRange, yRange, zRange)) }
    .foldLeft(Seq[Cube]()) { case (cubes, (mode, newCube)) =>
      val percentage = i * 100 / operations.size
      i += 1
      if (i % 10 == 0)
        println(s"... $percentage%: cubes: ${cubes.size}")
      val (fullyInside, otherCubes) = cubes.partition(cube => newCube containsAll cube)
      val (intersected, disjunct) = otherCubes.partition(cube => cube intersects newCube)
      val withoutNewCube = disjunct ++ intersected.flatMap(cube => cube subtract newCube)
      if (mode)
        withoutNewCube :+ newCube
      else
        withoutNewCube
    }

  println(s"part 2: ${result.map(_.size).sum}")
}