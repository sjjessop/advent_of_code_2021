object day03 {

  import day01.{readLines, checkAnswer}

  def frequentDigit(values: List[Char]) = {
    if (values.count(_ == '1') >= values.length / 2.0) '1' else '0'
  }
  def infrequentDigit(values: List[Char]) = if (frequentDigit(values) == '1') '0' else '1'

  def parseBinary(binary: String) = Integer.parseInt(binary, 2)

  // Only recurses as deep as the number of digits, which isn't many.
  // @annotation.tailrec
  def criterion(func: List[Char] => Char)(population: List[List[Char]]): List[Char] = {
    // println(population.length)
    val keepDigit = func(population.map(_.head))
    val keptValues = population.filter(_.head == keepDigit)
    if (keptValues.tail == Nil) keptValues.head else keepDigit::criterion(func)(keptValues.map(_.tail))
  }

  def test(): Unit = {
    val testData = List(
      "00100",
      "11110",
      "10110",
      "10111",
      "10101",
      "01111",
      "00111",
      "11100",
      "10000",
      "11001",
      "00010",
      "01010",
    ).map(_.toList)
    assert(criterion(frequentDigit)(testData).mkString == "10111")
    assert(criterion(infrequentDigit)(testData).mkString == "01010")
  }

  def main(args: Array[String]): Unit = {
    test()

    val diagnostics = readLines("day_03_input.txt").map(_.toList)
    val digits = diagnostics.map(_.length).max
    assert(digits==diagnostics.map(_.length).min)
    val gamma = diagnostics.transpose.map(frequentDigit).mkString
    val epsilon = gamma.map(ch => if (ch == '1') '0' else '1').mkString
    val result = parseBinary(gamma) * parseBinary(epsilon)
    checkAnswer(3, 1, result)

    val generator = criterion(frequentDigit)(diagnostics).mkString
    val scrubber = criterion(infrequentDigit)(diagnostics).mkString
    val result2 = parseBinary(generator) * parseBinary(scrubber)
    checkAnswer(3, 2, result2)
  }
}
