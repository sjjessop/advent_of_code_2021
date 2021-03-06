object day07 {

  import day01.{readLines, checkAnswer}

  def main(args: Array[String]): Unit = {
    val crabs = readLines("day_07_input.txt").flatMap(_.split(",")).map(_.toInt).sorted
    // The value that minimises the sum of absolute differences is the (in fact any) median
    val median = crabs(crabs.length / 2)
    val cost = crabs.map(x => (median - x).abs).sum
    checkAnswer(7, 1, cost)

    // New cost rule is quadratic, so maybe the mean is the correct answer? If not then it's close.
    val mean = crabs.sum / crabs.length
    val triCost = crabs.map(x => (mean - x).abs).map(n => (n * (n + 1)) / 2).sum
    // That was a cheap win.
    checkAnswer(7, 2, triCost)
  }
}
