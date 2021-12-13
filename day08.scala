object day08 {

  import day01.{readLines, checkAnswer}

  // Segments lit in each of 1, 4, 7, 8
  val uniqueSizes = Set(2, 4, 3, 7)

  class Display(digits: List[String], val output: List[String]) {
  }

  object Display {
    def parse(line: String) = {
      line.split(" \\| ").toList match {
        case (digitStr::outputStr::Nil) =>
          new Display(split(10)(digitStr), split(4)(outputStr))
        case _ => throw new Exception("bad")
      }
    }

    def split(size: Int)(line: String) = {
      val parts = line.split(" ")
      if (parts.length == size) parts.toList else throw new Exception("bad")
    }
  }

  def main(args: Array[String]): Unit = {
    val displays = readLines("day_08_input.txt").map(Display.parse)
    val result = displays
      .flatMap(_.output)
      .filter(x => uniqueSizes.contains(x.length))
      .length
    checkAnswer(8, 1, result)

  }
}
