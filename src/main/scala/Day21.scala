import utils.ResourceFile

import scala.collection.mutable

object Day21 extends App {

  val lines = ResourceFile.readLines("day21.txt")
  val pattern = "Player (\\d+) starting position: (\\d+)".r

  val startPositions = lines.map {
    case pattern(player, start) => (player.toInt, start.toInt)
  }.toMap

  val currentPositions = new mutable.HashMap() ++ startPositions
  val currentScores = new mutable.HashMap() ++ Map(1 -> 0, 2 -> 0)
  var diceValue = 1

  def nextDiceValue: Int = {
    val result = diceValue
    diceValue = (diceValue % 1000) + 1
    result
  }

  var i = 0
  while (currentScores(1) < 1000 && currentScores(2) < 1000) {
    val index = i % 2 + 1
    val newValue = currentPositions(index) + (nextDiceValue + nextDiceValue + nextDiceValue)
    currentPositions(index) = (newValue - 1) % 10 + 1
    currentScores(index) += currentPositions(index)
    i += 1
  }

  val result = Math.min(currentScores(1), currentScores(2)) * i * 3
  println(s"part 1: $result")

  // -------------

  case class Universe(positions: Map[Int, Int], scores: Map[Int, Int], player: Int) {
    def nextRound(dice: Int): Universe = {
      val position = (positions(player) + dice - 1) % 10 + 1
      val score = scores(player) + position
      Universe(
        positions + (player -> position),
        scores + (player -> score),
        player % 2 + 1
      )
    }
  }

  val diceCounts: List[(Int, Int)] =
    (for (a <- 1 to 3; b <- 1 to 3; c <- 1 to 3) yield a + b + c)
      .groupBy(identity)
      .map { case (roll, list) => (roll, list.size) }
      .toList

  def splitUniverse(universe: Universe, copies: Long): List[(Universe, Long)] = {
    diceCounts
      .map { case (dice, count) => (universe.nextRound(dice), copies * count) }
  }

  val startUniverse = Universe(startPositions, Map(1 -> 0, 2 -> 0), 1)
  var wipUniverses = Map(startUniverse -> 1L)
  val wonUniverses = new mutable.HashMap() ++ Map(1 -> 0L, 2 -> 0L)

  while (wipUniverses.nonEmpty) {
    val player = wipUniverses.head._1.player
    val (completed, inProgress) = wipUniverses.toList
      .flatMap { case (universe, count) => splitUniverse(universe, count) }
      .groupMap(_._1)(_._2)
      .map { case (universe, list) => (universe, list.sum) }
      .partition { case (universe, _) => universe.scores(player) >= 21 }
    wonUniverses(player) += completed.values.sum
    wipUniverses = inProgress
  }

  println(s"part 2: ${Math.max(wonUniverses(1), wonUniverses(1))}")
}
