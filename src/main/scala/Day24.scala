import utils.ResourceFile

object Day24 extends App {

  /*
  This solution is based on the following observations:
  - The input is divided in 14 blocks that have 18 lines each
  - Each block is dependent only on the z value of the previous block
  - Each block differs only in 3 lines. There are two types of blocks:
    - BlockType1 with div 1 in line 5. The first condition is always false, so it evaluates to a simple expression for z
    - BlockType2 with div 26 in line 5. It can evaluate to two different expressions
  - There are 7 pieces of each block type. Block1 roughly multiplies z by 26, therefore Block2 should divide z by 26,
    so that the end result converges to 0. Therefore the else branch of BlockType2.evaluate is ignored.
   */

  val blocks = ResourceFile.readLines("day24.txt")
    .map(_.split(" "))
    .sliding(18, 18)
    .toList

  sealed trait BlockType {
    def evaluate(w: Long, z: Long): Option[Long]
  }

  case class BlockType1(addX: Int, addY: Int) extends BlockType {
    override def evaluate(w: Long, z: Long): Option[Long] =
      Some(26 * z + w + addY)
  }

  case class BlockType2(addX: Int, addY: Int) extends BlockType {
    override def evaluate(w: Long, z: Long): Option[Long] =
      if (z % 26 + addX == w)
        Some(z / 26)
      else
        None // Some(z / 26 * 26 + w + addY)
  }

  val typedBlocks = blocks
    .map(block => block(4).last match {
      case "1" => BlockType1(block(5).last.toInt, block(15).last.toInt)
      case "26" => BlockType2(block(5).last.toInt, block(15).last.toInt)
    })

  def evaluateBlock(block: BlockType, zValues: Map[Long, (String, String)]): Map[Long, (String, String)] = {
    val results = for (
      w <- 1 to 9;
      (prevZ, (minCode, maxCode)) <- zValues;
      nextZ <- block.evaluate(w, prevZ)
    ) yield (nextZ, (minCode + w, maxCode + w))
    results
      .groupMap(_._1)(_._2)
      .map { case (key, values) => (key, (values.map(_._1).min, values.map(_._2).max)) }
  }

  val initial = Map(0L -> ("", ""))
  val zValues = typedBlocks
    .foldLeft(initial)((zValues, block) => evaluateBlock(block, zValues))

  val result = zValues(0L)
  println(s"part 1: ${result._2}")
  println(s"part 2: ${result._1}")
}
