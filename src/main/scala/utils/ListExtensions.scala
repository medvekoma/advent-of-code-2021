package utils

object ListExtensions {

  implicit class RichList[T](list: List[T]) {
    def splitBySeparator(separator: T): Seq[Seq[T]] =
      list.foldLeft(Seq(Seq.empty[T])) {
        (acc, s) =>
          if (s == separator) acc :+ Seq.empty
          else acc.init :+ (acc.last :+ s)
      }
  }

}
