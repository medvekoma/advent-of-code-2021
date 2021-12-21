import utils.ResourceFile

import scala.collection.mutable
import scala.collection.mutable.HashMap

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
    println(i, currentPositions(1), currentPositions(2))
  }

  val result = Math.min(currentScores(1), currentScores(2)) * i * 3
  println(result)
}
