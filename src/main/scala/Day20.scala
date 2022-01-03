import utils.MatrixExtensions.Matrix
import utils.ResourceFile

import scala.util.Try

object Day20 extends App {

  val lines = ResourceFile.readLines("day20.txt")

  def decodeLine(line: String): List[Int] = line.map {
    case '#' => 1
    case '.' => 0
  }.toList

  val enhancementString = decodeLine(lines.head)

  val inputImage = lines.tail.tail
    .map(decodeLine)

  def binaryToDecimal(binary: Seq[Int]): Int = {
    binary.foldRight((0, 1)) {
      case (num, (total, pow)) => (total + num * pow, 2 * pow)
    }._1
  }

  def enhancedPixel(image: List[List[Int]], row: Int, col: Int, default: Int): Int = {
    val indexSeq = for (
      r <- row - 1 to row + 1;
      c <- col - 1 to col + 1
    ) yield Try(image(r)(c)).getOrElse(default)
    val index = binaryToDecimal(indexSeq)
    enhancementString(index)
  }

  implicit class Image(image: List[List[Int]]) {
    def enhance(times: Int): List[List[Int]] = {
      (1 to times).foldLeft(image, 0) { case ((input, default), _) =>
        val output = Array.tabulate(input.rows + 2, input.cols + 2)(
          (row, col) => enhancedPixel(input, row - 1, col - 1, default)
        )
        val outputImage = output.map(_.toList).toList
        val nextDefault = enhancedPixel(image, -10000, -10000, default)
        (outputImage, nextDefault)
      }._1
    }

    def litPixels: Int = image.map(_.sum).sum
  }

  println(s"part 1: ${inputImage.enhance(times = 2).litPixels}")
  println(s"part 2: ${inputImage.enhance(times = 50).litPixels}")

}
