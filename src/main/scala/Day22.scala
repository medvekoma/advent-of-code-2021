import utils.ResourceFile

object Day22 extends App {

  val lines = ResourceFile.readLines("day22.txt")
  val pattern = "(\\w+) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)".r

  val operations = lines
    .collect { case pattern(mode, x1, x2, y1, y2, z1, z2) =>
      (mode == "on", Cube(Range(x1.toInt, x2.toInt), Range(y1.toInt, y2.toInt), Range(z1.toInt, z2.toInt)))
    }

  case class Range(min: Int, max: Int) {
    def nonEmpty: Boolean = min <= max

    def -(that: Range): Seq[Range] =
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

    def -(that: Cube): Seq[Cube] =
      ((this.xRange - that.xRange).map(xRange => Cube(xRange, this.yRange, this.zRange)) ++
        (this.yRange - that.yRange).map(yRange => Cube(this.xRange & that.xRange, yRange, this.zRange)) ++
        (this.zRange - that.zRange).map(zRange => Cube(this.xRange & that.xRange, this.yRange & that.yRange, zRange)))
        .filter(x => x.nonEmpty)

    def &(that: Cube): Cube =
      Cube(this.xRange & that.xRange, this.yRange & that.yRange, this.zRange & that.zRange)

    def intersects(that: Cube): Boolean =
      (this & that).nonEmpty

    def includes(that: Cube): Boolean =
      (this.xRange includes that.xRange) && (this.yRange includes that.yRange) && (this.zRange includes that.zRange)

    def size: Long =
      xRange.size.toLong * yRange.size * zRange.size
  }

  def reboot(limitedCube: Option[Cube] = None): Seq[Cube] =
    operations
      .foldLeft(Seq[Cube]()) { case (cubes, (mode, nextCube)) =>
        val newCube = limitedCube.map(_ & nextCube).getOrElse(nextCube)
        val notInNewCube = cubes.filterNot(cube => newCube includes cube)
        val (intersected, standalone) = notInNewCube.partition(cube => cube intersects newCube)
        val withoutNewCube = standalone ++ intersected.flatMap(cube => cube - newCube)
        if (mode)
          withoutNewCube :+ newCube
        else
          withoutNewCube
      }

  val initRange = Range(-50, 50)
  val initCube = Cube(initRange, initRange, initRange)
  val cubes1: Seq[Cube] = reboot(Some(initCube))
  val cubes2: Seq[Cube] = reboot()

  println(s"part 1: ${cubes1.map(_.size).sum}")
  println(s"part 2: ${cubes2.map(_.size).sum}")
}
