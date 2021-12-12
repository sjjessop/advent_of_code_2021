object day05 {

  import day01.{readLines, checkAnswer}

  sealed case class Pos(x: Int, y: Int) {
    def +(other: Direction) = Pos(x + other.x, y + other.y)
    def -(other: Pos) = Direction(x - other.x, y - other.y)
  }

  object Pos {
    def parse(data: String): Pos = {
      data.split(",").toList.map(_.toInt) match {
        case x::y::Nil => Pos(x, y)
        case _ => throw new Exception("bad")
      }
    }
  }

  case class Direction(x: Int, y: Int) {
    def *(other: Int): Direction = Direction(x * other, y * other)
    def sign = Direction(x.sign, y.sign)
  }

  case class Line(start: Pos, direction: Direction, size: Int) {
    assert(size >= 1)
    def end: Pos = start + (direction * (size - 1))
    def positions: Iterable[Pos] = {
      (0 until size).map(distance => start + direction * distance)
    }
  }

  object Line {
    def parseAny(line: String): Line = {
      line.split(" -> ").toList match {
        case startData::endData::Nil => {
          val start = Pos.parse(startData)
          val end = Pos.parse(endData)
          val diff = if (start.x != end.x) end.x - start.x else end.y - start.y
          Line(start, (end - start).sign, diff.abs + 1)
        }
        case _ => throw new Exception("bad")
      }
    }
    def parse(line: String): Option[Line] = {
      Some(parseAny(line)).filter(row => row.direction.x == 0 || row.direction.y == 0)
    }
  }

  def countMultiples(vents: List[Line]): Int = {
    vents.flatMap(_.positions).groupBy(identity).values.count(_.length > 1)
  }

  def test(): Unit = {
    val testVents = List(
      "0,9 -> 5,9",
      "8,0 -> 0,8",
      "9,4 -> 3,4",
      "2,2 -> 2,1",
      "7,0 -> 7,4",
      "6,4 -> 2,0",
      "0,9 -> 2,9",
      "3,4 -> 1,4",
      "0,0 -> 8,8",
      "5,5 -> 8,2",
    )
    assert(countMultiples(testVents.flatMap(Line.parse)) == 5)
    assert(countMultiples(testVents.map(Line.parseAny)) == 12)
  }

  def main(args: Array[String]): Unit = {
    test()

    val vents = readLines("day_05_input.txt")
    val result = countMultiples(vents.flatMap(Line.parse))
    checkAnswer(5, 1, result)

    val result2 = countMultiples(vents.map(Line.parseAny))
    checkAnswer(5, 2, result2)
  }
}
