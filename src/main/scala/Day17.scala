import utils.ResourceFile
import utils.TupleExtensions._

import scala.annotation.tailrec

object Day17 extends App {

  val lines = ResourceFile.readLines("day17.txt")

  val pattern = "^target.+: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)".r
  lines
    .collect { case pattern(x1, x2, y1, y2) => new Playground(x1.toInt, x2.toInt, y1.toInt, y2.toInt) }
    .foreach(_.play())

  class Playground(tx1: Int, tx2: Int, ty1: Int, ty2: Int) {
    def play(): Unit = {
      val velocities = for (vx <- vxRange(); vy <- vyRange()) yield (vx, vy)
      val hits = velocities
        .map(v => (v, shoot(v)))
        .collect { case (v, Hit(maxHeight)) => (v, maxHeight) }
      val part1 = hits.maxBy(_._2)
      println(s"part 1: Velocity ${part1._1} reaches to maximum height of ${part1._2}.")
      println(s"part 2: There are ${hits.size} hits.")
    }

    def vxRange(): Seq[Int] = {
      var vx = 1
      while (vx * (vx + 1) / 2 < tx1) vx += 1
      vx to tx2
    }

    def vyRange(): Seq[Int] = -Math.abs(ty1) - 1 to Math.abs(ty1) + 1

    sealed trait ShootingState
    case class Hit(maxHeight: Int) extends ShootingState
    case object Hit extends ShootingState
    case object Miss extends ShootingState
    case object InProgress extends ShootingState

    def shootingState(position: (Int, Int)): ShootingState = {
      val (x, y) = position
      if ((tx1 to tx2).contains(x) && (ty1 to ty2).contains(y))
        Hit
      else if (x > tx2 || y < ty1)
        Miss
      else
        InProgress
    }

    def shoot(velocity: (Int, Int)): ShootingState = {

      @tailrec
      def trajectory(position: (Int, Int), velocity: (Int, Int), maxY: Int = 0): ShootingState = {
        shootingState(position) match {
          case Hit => Hit(maxY)
          case Miss => Miss
          case InProgress =>
            trajectory(position + velocity, newVelocity(velocity), Math.max(position._2, maxY))
        }
      }

      def newVelocity(velocity: (Int, Int)): (Int, Int) = {
        val (vx, vy) = velocity
        (Math.max(vx - 1, 0), vy -1)
      }

      trajectory((0, 0), velocity)

    }
  }
}
