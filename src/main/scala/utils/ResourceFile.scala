package utils

import scala.io.Source
import scala.util.Using

object ResourceFile {

  def readLines(fileName: String): List[String] =
    Using(Source.fromResource(fileName)) {
      _.getLines().toList
    }.get

}
