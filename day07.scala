object day07 {

  import day01.{readLines, checkAnswer}

  def main(args: Array[String]): Unit = {
    val crabs = readLines("day_07_input.txt").flatMap(_.split(",")).map(_.toInt).sorted
    // The value that minimises the sum of absolute differences is the (in fact any) median
    val median = crabs(crabs.length / 2)
    val cost = crabs.map(x => (median - x).abs).sum
    checkAnswer(7, 1, cost)

  }
}
