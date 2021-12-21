object day20 {

  import day01.{readLines, checkAnswer}
  import day05.{Pos, Direction}

  val neighbours = List(
    Direction(-1, -1), Direction(0, -1), Direction(1, -1),
    Direction(-1, 0), Direction(0, 0), Direction(1, 0),
    Direction(-1, 1), Direction(0, 1), Direction(1, 1),
  )

  def surround(cells: Iterable[Pos], left: Int, right: Int, top: Int, bottom: Int): Iterable[Pos] = {
    val topRow = for (x <- left - 1 to right) yield Pos(x, top - 1)
    val bottomRow = for (x <- left - 1 to right) yield Pos(x, bottom)
    val leftEdge = for (y <- top - 1 to bottom) yield Pos(left - 1, y)
    val rightEdge = for (y <- top - 1 to bottom) yield Pos(right, y)
    cells ++ topRow ++ bottomRow ++ leftEdge ++ rightEdge
  }

  // It looks like you're writing a cellular automaton. Can I help you with that?
  sealed case class Window(program: Seq[Int], cells: Map[Pos, Int]) {
    assert(program.size == 512)
    val left = cells.map(_._1.x).min
    val right = cells.map(_._1.x).max + 1
    val width = right - left
    // y co-ordinate increases downwards from the top
    val top = cells.map(_._1.y).min
    val bottom = cells.map(_._1.y).max + 1
    val height = bottom - top
    assert(cells.size == width * height)

    def display: String = {
      val lines = for (y <- top until bottom) yield {
        val poses = for(x <- left until right) yield Pos(x, y)
        poses.map(cells).map(v => if (v == 1) '#' else '.').mkString
      }
      lines.mkString("\n")
    }

    def hood(pos: Pos): List[Int] = neighbours.map(pos + _).map(cells)
    def hoodValue(pos: Pos): Int = hood(pos).foldLeft(0)((accum, digit) => 2 * accum + digit)
    def nextAt(pos: Pos): Int = program(hoodValue(pos))

    @annotation.tailrec
    def step(n: Int): Window = {
      if (n <= 0) this
      else {
        val background = nextAt(Pos(Int.MaxValue - 1, Int.MaxValue - 1))
        val nextRegion = surround(cells.keys, left, right, top, bottom)
        val nextCells = nextRegion
          .map(pos => (pos -> nextAt(pos)))
          .toMap
          .withDefaultValue(background)
        Window(program, nextCells).step(n-1)
      }
    }

    def litCount: Int = cells.values.count(_ == 1)
  }

  def lit(ch: Char): Int = ch match {
    case '.' => 0
    case '#' => 1
    case _ => throw new Exception("bad")
  }

  object Window {
    def parse(lines: List[String], offset: Int = 0) = {
      val program = lines.head.toSeq.map(lit)
      val cells = for (
        (line, yIdx) <- lines.drop(2).zipWithIndex;
        (cell, xIdx) <- line.zipWithIndex
      ) yield (Pos(xIdx + offset, yIdx + offset) -> lit(cell))
      Window(program, cells.toMap.withDefaultValue(0))
    }
  }

  def test(): Unit = {
    val testWindow = Window.parse(List(
      List(
        "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##",
        "#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###",
        ".######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.",
        ".#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....",
        ".#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..",
        "...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....",
        "..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#",
      ).mkString,
      "",
      "#..#.",
      "#....",
      "##..#",
      "..#..",
      "..###",
    ))
    val middle = Pos(2,2)
    assert(testWindow.hoodValue(middle) == 34)
    assert(testWindow.nextAt(middle) == 1)
    assert(testWindow.step(2).litCount == 35)

    /*
    println(testWindow.display)
    println("-------------------------")
    println(testWindow.step(1).display)
    println("-------------------------")
    println(testWindow.step(2).display)
    */
  }

  def main(args: Array[String]): Unit = {
    test()

    val window = Window.parse(readLines("day_20_input.txt"))
    val result = window.step(2).litCount
    checkAnswer(20, 1, result)
  }
}
