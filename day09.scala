object day09 {

  import day01.{readLines, checkAnswer}

  sealed case class Point(row: Int, col: Int)(implicit heightMap: Seq[Seq[Int]]) {
    lazy val height: Int = heightMap(row)(col)

    def neighbours = List(
      Option.when(row > 0)(Point(row - 1, col)),
      Option.when(col > 0)(Point(row, col - 1)),
      Option.when(row < heightMap.length - 1)(Point(row + 1, col)),
      Option.when(col < heightMap(0).length - 1)(Point(row, col + 1)),
    ).flatten

    def isLow: Boolean = !neighbours.map(_.height <= height).contains(true)

    @annotation.tailrec
    def floodFill(start: Set[Point] = Set(this)): Set[Point] = {
      val expanded = (start union start.flatMap(_.neighbours)).filter(_.height != 9)
      if (expanded.size == start.size) start else floodFill(expanded)
    }

    def basinSize: Int = floodFill().size
  }

  object Point {
    def apply(row: Int, col: Int)(implicit heightMap: Seq[Seq[Int]]): Point = new Point(row, col)
    def all(implicit heightMap: Seq[Seq[Int]]): Iterable[Point] = (
      for (row <- 0 until heightMap.length; col <- 0 until heightMap(0).length)
      yield Point(row, col)
    )
  }

  def main(args: Array[String]): Unit = {
    implicit val heightMap = readLines("day_09_input.txt").toSeq.map(r => r.toSeq.map(_.toString.toInt))
    val widths = heightMap.map(_.length)
    assert(widths.max == widths.min)
    val lowpoints = Point.all.filter(_.isLow)
    val total = lowpoints.map(_.height + 1).sum
    checkAnswer(9, 1, total)

    // Basins seem a bit under-specified, since it doesn't say which basin the
    // 8 belongs to in this layout: 080. Let's proceed as if every low point
    // is separated by 9s, and hope the answer comes out right.
    val basins = lowpoints.map(_.basinSize).toSeq.sorted.takeRight(3).product
    checkAnswer(9, 2, basins)
  }
}
