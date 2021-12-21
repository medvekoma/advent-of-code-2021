import utils.ResourceFile

import scala.collection.mutable

object Day21b extends App {

  val lines = ResourceFile.readLines("day21.txt")
  val pattern = "Player (\\d+) starting position: (\\d+)".r

  val startPositions = lines.map {
    case pattern(player, start) => (player.toInt, start.toInt)
  }.toMap

  case class Universe(positions: Map[Int, Int], scores: Map[Int, Int], throwCount: Int, copies: Long, winnerId: Int)

  val diceMap: Map[Int, Int] = (for (a <- 1 to 3; b <- 1 to 3; c <- 1 to 3) yield a + b + c)
    .groupBy(identity)
    .map { case (num, list) => (num, list.size) }

  def advance(universe: Universe, sum: Int, num: Int, player: Int): Universe = {
    val position = (universe.positions(player) + sum - 1) % 10 + 1
    val score = universe.scores(player) + position
    val winnerId = if (score >= 27) player else universe.winnerId
    Universe(
      universe.positions + (player -> position),
      universe.scores + (player -> score),
      universe.throwCount + 3,
      universe.copies * num,
      winnerId
    )
  }

  val startUniverse = Universe(startPositions, Map(1 -> 0, 2 -> 0), throwCount = 0, copies = 1L, winnerId = 0)
  var wipUniverses = List(startUniverse)
  val wonUniverses = new mutable.HashMap() ++ Map(1 -> 0L, 2 -> 0L)
  while (wipUniverses.nonEmpty) {
    println(s"... wipUniverses: ${wipUniverses.size}")
    val player = wipUniverses.head.throwCount % 6 match {
      case n if n < 3 => 1
      case _ => 2
    }
    val newUniverses = wipUniverses.flatMap { universe =>
      diceMap.map { case (sum, num) => advance(universe, sum, num, player) }
    }.groupBy(_.winnerId)
    wonUniverses(1) += newUniverses.getOrElse(1, List.empty).size
    wonUniverses(2) += newUniverses.getOrElse(2, List.empty).size
    wipUniverses = newUniverses.getOrElse(0, List.empty)
      .groupBy(identity)
      .map { case (universe, list) => (universe, list.size) }
      .map { case (universe, count) => universe.copy(copies = universe.copies * count)}
      .toList
    println(s"... ... won: $wonUniverses")
  }

  println(wonUniverses)

  // 16942361
  // 15835895

  // 54793422
  // 51507645

}
