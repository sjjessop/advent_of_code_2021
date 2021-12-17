object day13 {

  import day01.{readLines, checkAnswer}
  import day05.Pos

  case class Manual(points: Set[Pos], folds: List[String]) {
    def foldX(idx: Int): Set[Pos] = {
      points.filter(_.x < idx).union(
        points.filter(_.x > idx).map{case Pos(x, y) => Pos(2 * idx - x, y)}
      )
    }
    def foldNext: Manual = {
      val instruction = folds.head
      if (instruction("fold along ".size) == 'x')
        Manual(foldX(instruction.substring("fold along x=".size).toInt), folds.tail)
      else ???
    }
  }

  object Manual {
    def parse(lines: List[String]): Manual = {
      def isFold(line: String) = line != "" && line.substring(0, 4) == "fold"
      def notFold(line: String) = line != "" && !isFold(line)
      Manual(
        lines.filter(notFold).map(Pos.parse).toSet,
        lines.filter(isFold)
      )
    }
  }

  def main(args: Array[String]): Unit = {
    val manual = Manual.parse(readLines("day_13_input.txt"))
    val firstFoldShowing = manual.foldNext.points.size
    checkAnswer(13, 1, firstFoldShowing)
  }
}
