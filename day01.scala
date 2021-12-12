object day01 {

  def readLines(filename: String): List[String] = {
    val source = io.Source.fromFile(filename)
    try
      source.getLines().toList
    finally
      source.close()
  }

  def checkAnswer(day: Int, part: Int, answer: Long) = {
    println(answer)
    val dayString = "%02d".format(day)
    val lines = readLines(s"day_${dayString}_answers.txt")
    assert(lines(part-1).toLong == answer)
  }

  def main(args: Array[String]): Unit = {
    val depths = readLines("day_01_input.txt").map(_.toInt)
    def isDrop(vals: List[Int]) = vals(0) < vals(1)
    val drops = depths.sliding(2).count(isDrop _)
    checkAnswer(1, 1, drops)

    val windowDrops = depths.sliding(3).map(_.sum).toList.sliding(2).count(isDrop _)
    checkAnswer(1, 2, windowDrops)
  }
}
