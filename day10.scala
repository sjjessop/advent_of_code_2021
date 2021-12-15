object day10 {

  import day01.{readLines, checkAnswer}

  def expect(ch: Char, open: List[Char], bad: Char): Either[Char, List[Char]] = open match {
    case Nil => throw new Exception("bad")
    case `ch`::t => Right(t)
    case _ => Left(bad)
  }

  def opener(ch: Char) = ch match {
    case ')' => '('
    case ']' => '['
    case '}' => '{'
    case '>' => '<'
    case _ => throw new Exception("bad")
  }

  class Line(content: String) {
    lazy val checked = {
      val chars = content.toList
      // Either a bad character, or the current list of open brackets
      val start: Either[Char, List[Char]] = Right(Nil)
      chars.foldLeft(start)((sofar, next) => sofar match {
        case Left(ch) => Left(ch)
        case Right(open) => next match {
          case '(' | '[' | '{' | '<' => Right(next::open)
          case _ => expect(opener(next), open, next)
        }
      })
    }

    def badChar: Option[Char] = checked match {
      case Left(ch) => Some(ch)
      case _ => None
    }
  }

  object Line {
    def parse(content: String) = new Line(content)
  }

  val scores = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
  )

  def main(args: Array[String]): Unit = {
    val lines = readLines("day_10_input.txt")
      .map(Line.parse)

    val totalScore = lines
      .flatMap(_.badChar)
      .map(scores)
      .sum
    checkAnswer(10, 1, totalScore)
  }
}
