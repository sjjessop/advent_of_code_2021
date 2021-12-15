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

  def closer(ch: Char) = ch match {
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
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

    def closing: Option[String] = checked match {
      case Left(ch) => None
      case Right(opens) => Some(opens.map(closer).mkString)
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

  val scores2 = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4,
  )

  def closingScore(value: String) = value.foldLeft(0L)((l, r) => 5L * l + scores2(r))

  def getCloseScore(ls: List[Line]) = {
    val closeStrings = ls.flatMap(_.closing).filter(_ != "")
    val closeScores = closeStrings.map(closingScore).toSeq.sorted
    closeScores(closeScores.length / 2)
  }

  def test(): Unit = {
    val testLines = List(
      "[({(<(())[]>[[{[]{<()<>>",
      "[(()[<>])]({[<{<<[]>>(",
      "{([(<{}[<>[]}>{[]{[(<()>",
      "(((({<>}<{<{<>}{[]{[]{}",
      "[[<[([]))<([[{}[[()]]]",
      "[{[{({}]{}}([{[{{{}}([]",
      "{<[[]]>}<{[{[{[]{()[[[]",
      "[<(<(<(<{}))><([]([]()",
      "<{([([[(<>()){}]>(<<{{",
      "<{([{{}}[<[[[<>{}]]]>[]]",
    ).map(Line.parse)
    assert(getCloseScore(testLines) == 288957)
  }

  def main(args: Array[String]): Unit = {
    val lines = readLines("day_10_input.txt")
      .map(Line.parse)

    val totalScore = lines
      .flatMap(_.badChar)
      .map(scores)
      .sum
    checkAnswer(10, 1, totalScore)

    test()

    val medianScore = getCloseScore(lines)
    checkAnswer(10, 2, medianScore)
  }
}
