object day06 {

  import day01.{readLines, checkAnswer}

  class School(val fish: Map[Int, Long]) {
    // println(fish)
    def evolved: School = {
      val parents = fish.getOrElse(0, 0L)
      val seveners = fish.getOrElse(7, 0L)
      if ((seveners + parents) >= (1L << 58)) throw new Exception("approaching overflow")
      new School(
        (fish - 0)
        .map{case (k, v) => (k-1, v)}
        + (6 -> (seveners + parents))
        + (8 -> parents)
      )
    }
    def total: Long = fish.values.sum
  }

  object School {
    def apply(ages: List[Int]) = new School(ages.groupBy(identity).view.mapValues(_.size.toLong).toMap)
  }

  def main(args: Array[String]): Unit = {
    val ages = readLines("day_06_input.txt").flatMap(_.split(",")).map(_.toInt)
    val school = School(ages)
    val result = (1 to 80).foldLeft(school)((s, idx) => s.evolved).total
    checkAnswer(6, 1, result)

    val result2 = (1 to 256).foldLeft(school)((s, idx) => s.evolved).total
    checkAnswer(6, 2, result2)
  }
}
