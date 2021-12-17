package utils

object TupleExtensions {

  implicit class IntPair(pair: (Int, Int)) {
    def +(other: (Int, Int)): (Int, Int) =
      (pair._1 + other._1, pair._2 + other._2)
  }

}
