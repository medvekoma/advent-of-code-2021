import utils.ResourceFile

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
    val cubes = for(x <- xRange; y <- yRange; z <- zRange) yield (x, y, z)
    if (operation.mode)
      cubesOn ++= cubes.toSet
    else
      cubesOn --= cubes.toSet
  }

  println(cubesOn.size)

}
