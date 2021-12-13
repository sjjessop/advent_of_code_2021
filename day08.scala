object day08 {

  import day01.{readLines, checkAnswer}

  // Segments lit in each of 1, 4, 7, 8
  val uniqueSizes = Set(2, 4, 3, 7)

  class Display(digits: List[String], val output: List[String]) {

    def getUniqueDigit(pred: String => Boolean): String = {
      val matches = digits.filter(pred)
      assert(matches.length == 1)
      matches.head
    }

    def getDigitByLength(n: Int): String = getUniqueDigit(_.size == n)

    def uniqueDifference(left: String, right: String): Char = {
      val diff = left.toSet diff right.toSet
      assert(diff.size == 1)
      diff.head
    }

    def intersection(left: String, right: String): Set[Char] = left.toSet intersect right.toSet

    def solve: Int = {
      val digit1: String = getDigitByLength(2)
      val digit4: String = getDigitByLength(4)
      val digit7: String = getDigitByLength(3)
      val digit8: String = getDigitByLength(7)
      // Of the six-segment digits, 6 has f lit but not c, and the others (0 and 9) have both c and f lit
      val digit6: String = getUniqueDigit(x => x.size == 6 && intersection(x, digit1).size == 1)
      val segmentF: Char = intersection(digit6, digit1).head
      val segmentC: Char = uniqueDifference(digit1, segmentF.toString)
      // Of the five-segment digits, 3 has f and c both lit, and the others (2 and 5) have one but not the other.
      val digit3: String = getUniqueDigit(x => x.size == 5 && intersection(x, digit1).size == 2)
      val digit2: String = getUniqueDigit(x => x.size == 5 && x != digit3 && x.contains(segmentC))
      val digit5: String = getUniqueDigit(x => x.size == 5 && x != digit3 && x != digit2)
      // Finally we need to distinguish 0 from 9
      val segmentE: Char = uniqueDifference(digit2, digit3)
      val digit0: String = getUniqueDigit(x => x.size == 6 && x != digit6 && x.contains(segmentE))
      val digit9: String = getUniqueDigit(x => x.size == 6 && x != digit6 && x != digit0)
      // Not needed
      // val segmentA: Char = uniqueDifference(digit7, digit1)
      // val segmentB: Char = uniqueDifference(digit5, digit3)
      // val segmentD: Char = uniqueDifference(digit8, digit0)
      // val segmentG: Char = uniqueDifference(digit8, List(segmentA, segmentB, segmentC, segmentD, segmentE, segmentF).mkString)
      val code = Map(
        digit0.sorted -> 0, digit1.sorted -> 1, digit2.sorted -> 2, digit3.sorted -> 3,
        digit4.sorted -> 4, digit5.sorted -> 5, digit6.sorted -> 6, digit7.sorted -> 7,
        digit8.sorted -> 8, digit9.sorted -> 9,
      )
      output.map(x => code(x.sorted)).foldLeft(0)((acc, d) => acc * 10 + d)
    }
  }

  object Display {
    def parse(line: String) = {
      line.split(" \\| ").toList match {
        case (digitStr::outputStr::Nil) =>
          new Display(split(10)(digitStr), split(4)(outputStr))
        case _ => throw new Exception("bad")
      }
    }

    def split(size: Int)(line: String) = {
      val parts = line.split(" ")
      if (parts.length == size) parts.toList else throw new Exception("bad")
    }
  }

  def main(args: Array[String]): Unit = {
    val displays = readLines("day_08_input.txt").map(Display.parse)
    val result = displays
      .flatMap(_.output)
      .filter(x => uniqueSizes.contains(x.length))
      .length
    checkAnswer(8, 1, result)

    val total = displays.map(_.solve).sum
    checkAnswer(8, 2, total)
  }
}
