object day13 {

  import day01.{readLines, checkAnswer}
  import day05.Pos

  sealed case class Manual(points: Set[Pos], folds: List[String]) {
    def foldX(idx: Int): Set[Pos] = {
      points.filter(_.x < idx).union(
        points.filter(_.x > idx).map{case Pos(x, y) => Pos(2 * idx - x, y)}
      )
    }
    def foldY(idx: Int): Set[Pos] = {
      points.filter(_.y < idx).union(
        points.filter(_.y > idx).map{case Pos(x, y) => Pos(x, 2 * idx - y)}
      )
    }
    def foldNext: Manual = {
      val instruction = folds.head
      if (instruction("fold along ".size) == 'x')
        Manual(foldX(instruction.substring("fold along x=".size).toInt), folds.tail)
      else
        Manual(foldY(instruction.substring("fold along x=".size).toInt), folds.tail)
    }
    @annotation.tailrec
    def foldAll: Manual = folds match {
      case Nil => this
      case _ => this.foldNext.foldAll
    }
    def vis(): Seq[String] = {
      val width = points.map(_.x).max
      val height = points.map(_.y).max
      (0 to height).map(row =>
        (0 to width)
        .map(x => if (points contains Pos(x, row)) '*' else ' ')
        .mkString
      )
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

    val code = manual.foldAll.vis()
    println(code.mkString("\n"))
    def chomp(s: String) = s.reverse.dropWhile(_ == ' ').reverse
    assert(code.map(chomp) == readLines("day_13_answers.txt").drop(1).map(chomp))
  }
}
