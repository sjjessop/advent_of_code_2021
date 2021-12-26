object day25 {

  import day01.{readLines, checkAnswer}
  import day05.{Pos, Direction}

  val south = Direction(0, 1)
  val east = Direction(1, 0)

  case class Board(content: Map[Pos, Char], width: Int, height: Int) {
    // Check rectangular
    assert(content.keySet == (for(xIdx <- 0 until width; yIdx <- 0 until height) yield Pos(xIdx, yIdx)).toSet)

    def advance: Board = moveEast().moveSouth()
    def moveSouth() = move(south, 'v')
    def moveEast() = move(east, '>')

    def move(dir: Direction, what: Char) = {
      def move(pos: Pos) = wrap(pos + dir)
      val movable = content.keys.filter(pos => content(pos) == what && content(move(pos)) == '.')
      this.copy(content =
        (content ++ movable.map(pos => (pos, '.')) ++ movable.map(pos => (move(pos), what))).toMap
      )
    }

    def wrap(pos: Pos) = {
      if (pos.x >= width) Pos(0, pos.y)
      else if (pos.y >= height) Pos(pos.x, 0)
      else pos
    }

    lazy val lockedStep = Iterator.unfold(this){ b => {
      val newBoard = b.advance
      if (newBoard == b) None else Some((newBoard, newBoard))
    }}.count(b => true) + 1
  }

  object Board {
    def parse(lines: List[String]): Board = Board(
      (for (
        (line, yIdx) <- lines.zipWithIndex;
        (cell, xIdx) <- line.zipWithIndex
      ) yield (Pos(xIdx, yIdx) -> cell)).toMap,
      lines(0).size,
      lines.size,
    )
  }

  def test(): Unit = {
    assert(Board.parse(List("...>>>>>...")).advance == Board.parse(List("...>>>>.>..")))
    assert(Board.parse(List("...>>>>>...")).advance.advance == Board.parse(List("...>>>.>.>.")))
    assert(Board.parse(List(">>>")).lockedStep == 1)

    assert(Board.parse(List(
      "..........",
      ".>v....v..",
      ".......>..",
      "..........",
    )).advance == Board.parse(List(
      "..........",
      ".>........",
      "..v....v>.",
      "..........",
    )))

    val board = Board.parse(List(
      "v...>>.vv>",
      ".vv>>.vv..",
      ">>.>v>...v",
      ">>v>>.>.v.",
      "v>v.vv.v..",
      ">.>>..v...",
      ".vv..>.>v.",
      "v.v..>>v.v",
      "....v..v.>",
    ))

    assert(board.advance == Board.parse(List(
      "....>.>v.>",
      "v.v>.>v.v.",
      ">v>>..>v..",
      ">>v>v>.>.v",
      ".>v.v...v.",
      "v>>.>vvv..",
      "..v...>>..",
      "vv...>>vv.",
      ">.v.v..v.v",
    )))
    assert(board.lockedStep == 58)
  }

  def main(args: Array[String]): Unit = {
    test()

    val board = Board.parse(readLines("day_25_input.txt"))
    val result = board.lockedStep
    checkAnswer(25, 1, result)
  }
}
