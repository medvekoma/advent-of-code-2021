import utils.ResourceFile

object Day22 extends App {

  val lines = ResourceFile.readLines("day22.txt")
  val pattern = "(\\w+) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)".r

  val operations = lines.collect { case pattern(mode, x1, x2, y1, y2, z1, z2) =>
    (mode == "on", Cube(Range(x1.toInt, x2.toInt), Range(y1.toInt, y2.toInt), Range(z1.toInt, z2.toInt)))
  }

  case class Range(min: Int, max: Int) {
    def nonEmpty: Boolean = min <= max

    def subtract(that: Range): Seq[Range] =
      if (that.nonEmpty && this.nonEmpty)
        Seq(Range(this.min, that.min - 1), Range(that.max + 1, this.max))
      else {
        Seq(this)
      }.filter(_.nonEmpty)

    def &(that: Range): Range =
      Range(Math.max(this.min, that.min), Math.min(this.max, that.max))

    def includes(that: Range): Boolean =
      that.min >= this.min && that.max <= this.max

    def size: Int = Math.max(max - min + 1, 0)
  }

  case class Cube(xRange: Range, yRange: Range, zRange: Range) {
    def nonEmpty: Boolean =
      xRange.nonEmpty && yRange.nonEmpty && zRange.nonEmpty

    def subtract(that: Cube): Seq[Cube] =
      (
        (this.xRange subtract that.xRange).map(xRange => Cube(xRange, this.yRange, this.zRange)) ++
          (this.yRange subtract that.yRange).map(yRange => Cube(this.xRange & that.xRange, yRange, this.zRange)) ++
          (this.zRange subtract that.zRange).map(zRange => Cube(this.xRange & that.xRange, this.yRange & that.yRange, zRange))
        ).filter(x => x.nonEmpty)

    def &(that: Cube): Cube =
      Cube(this.xRange & that.xRange, this.yRange & that.yRange, this.zRange & that.zRange)

    def intersects(that: Cube): Boolean =
      (this & that).nonEmpty

    def size: Long =
      xRange.size.toLong * yRange.size * zRange.size

    def includes(that: Cube): Boolean =
      this.xRange.includes(that.xRange) && this.yRange.includes(that.yRange) && this.zRange.includes(that.zRange)
  }

  def reboot(validCube: Option[Cube] = None): Seq[Cube] =
    operations
      .foldLeft(Seq[Cube]()) { case (cubes, (mode, nextCube)) =>
        val newCube = validCube.map(_ & nextCube).getOrElse(nextCube)
        val notIncluded = cubes.filterNot(cube => newCube includes cube)
        val (intersected, distinct) = notIncluded.partition(cube => cube intersects newCube)
        val withoutNewCube = distinct ++ intersected.flatMap(cube => cube subtract newCube)
        if (mode)
          withoutNewCube :+ newCube
        else
          withoutNewCube
      }

  val validRange = Range(-50, 50)
  val validCube = Cube(validRange, validRange, validRange)
  val cubes1: Seq[Cube] = reboot(Some(validCube))
  val cubes2: Seq[Cube] = reboot()
  println(s"part 1: ${cubes1.map(_.size).sum}")
  println(s"part 2: ${cubes2.map(_.size).sum}")
}