object day03 {

  import day01.{readLines, checkAnswer}

  def frequentDigit(values: List[Char]) = {
    if (values.count(_ == '1') >= values.length / 2) '1' else '0'
  }

  def parseBinary(binary: String) = Integer.parseInt(binary, 2)

  def main(args: Array[String]): Unit = {
    val diagnostics = readLines("day_03_input.txt").map(_.toList)
    val digits = diagnostics.map(_.length).max
    assert(digits==diagnostics.map(_.length).min)
    val gamma = diagnostics.transpose.map(frequentDigit).mkString
    val epsilon = gamma.map(ch => if (ch == '1') '0' else '1').mkString
    val result = parseBinary(gamma) * parseBinary(epsilon)
    checkAnswer(3, 1, result)
  }
}
