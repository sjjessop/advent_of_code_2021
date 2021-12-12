object day04 {

  import day01.{readLines, checkAnswer}

  type Cell = Option[Int]
  def countMarks(row: List[Cell]) = row.count(_ == None)
  def hasWinner(rows: List[List[Cell]]): Boolean = rows.map(countMarks).contains(5)

  def markRow(row: List[Cell], n: Int): List[Cell] = {
    def replace(n: Int)(value: Cell): Cell =
      value.flatMap(v => if (v != n) Some(v) else None)
    row.map(replace(n))
  }

  case class Board(rows: List[List[Cell]]) {
    assert(rows.length == 5)
    assert(rows.map(_.length).toSet == Set(5))
    def mark(n: Int): Board = Board(rows.map(markRow(_, n)))
    def hasWon: Boolean = hasWinner(rows) || hasWinner(rows.transpose)
    def score: Int = rows.flatten.flatten.sum
  }

  object Board {
    def parse(rows: List[String]): Board = {
      def parseRow(row: String): List[Option[Int]] =
        row.split(" ").filter(_ != "").map(x => Some(x.toInt)).toList
      Board(rows.map(parseRow))
    }
  }

  def parseBingo(lines: List[String]): (List[Int], List[Board]) = (
    lines.head.split(",").map(_.toInt).toList,
    lines.tail.grouped(6).map(_.tail).map(Board.parse).toList,
  )

  def main(args: Array[String]): Unit = {
    val (draws, boards) = parseBingo(readLines("day_04_input.txt"))
    // println(boards.head.mark(33))
    @annotation.tailrec
    def findWinner(draws: List[Int], boards: List[Board]): (Board, Int) = {
      val draw = draws.head
      val newBoards = boards.map(_.mark(draw))
      val wonBoards = newBoards.filter(_.hasWon)
      if (wonBoards != Nil) (wonBoards.head, draw) else findWinner(draws.tail, newBoards)
    }
    val (winner, draw) = findWinner(draws, boards)
    val result = winner.score * draw
    checkAnswer(4, 1, result)

    @annotation.tailrec
    def findLastWinner(draws: List[Int], boards: List[Board]): (Board, Int) = {
      val draw = draws.head
      val newBoards = boards.map(_.mark(draw))
      val liveBoards = newBoards.filter(!_.hasWon)
      if (liveBoards == Nil) (newBoards.head, draw) else findLastWinner(draws.tail, liveBoards)
    }
    val (loser, draw2) = findLastWinner(draws, boards)
    val result2 = loser.score * draw2
    checkAnswer(4, 2, result2)
  }
}
