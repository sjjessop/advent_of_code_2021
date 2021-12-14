object day09 {

  import day01.{readLines, checkAnswer}

  def isLowPoint(rowIdx: Int, colIdx: Int, height: Seq[Seq[Int]]): Boolean = {
    val self = height(rowIdx)(colIdx)
    if (rowIdx > 0 && height(rowIdx - 1)(colIdx) <= self) false
    else if (colIdx > 0 && height(rowIdx)(colIdx - 1) <= self) false
    else if (rowIdx < height.length - 1 && height(rowIdx + 1)(colIdx) <= self) false
    else if (colIdx < height(0).length - 1 && height(rowIdx)(colIdx + 1) <= self) false
    else true
  }

  def main(args: Array[String]): Unit = {
    val heightMap = readLines("day_09_input.txt").toSeq.map(r => r.toSeq.map(_.toString.toInt))
    val widths = heightMap.map(_.length)
    val width = widths.max
    assert(width == widths.min)
    val scores = for (rowIdx <- 0 until heightMap.length; colIdx <- 0 until width)
      yield (if (isLowPoint(rowIdx, colIdx, heightMap)) heightMap(rowIdx)(colIdx) + 1 else 0)
    val total = scores.sum
    checkAnswer(9, 1, total)
  }
}
