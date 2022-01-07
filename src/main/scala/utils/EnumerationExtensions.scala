package utils

object EnumerationExtensions {

  implicit class RichSeq[T](seq: Seq[T]) {

    def splitBySeparator(separator: T): Seq[Seq[T]] =
      seq.foldLeft(Seq(Seq.empty[T])) {
        (acc, s) =>
          if (s == separator) acc :+ Seq.empty
          else acc.init :+ (acc.last :+ s)
      }

    def takeRightWhile(func: T => Boolean): Seq[T] =
      (seq.length - 1 to 0 by -1).iterator
        .map(index => seq(index))
        .takeWhile(func)
        .toSeq
  }
}
