package utils

object Measure {

  def dumpTime[T]()(body: => T): T = {
    val start = System.nanoTime()
    val res = body
    val executionTimeInMs = (System.nanoTime() - start) / 1e6
    println(s"Execution time: ${executionTimeInMs} ms.")
    res
  }

}
