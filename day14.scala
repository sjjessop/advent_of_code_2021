object day14 {

  import day01.{readLines, checkAnswer}

  case class Rule(left: String, right: String) {}

  object Rule {
    def parse(value: String): Rule = value.split(" -> ").toList match {
      case left::right::Nil => Rule(left, right)
      case _ => throw new Exception("bad")
    }
  }

  type RuleMap = Map[String, String]

  def makeRules(rules: Iterable[Rule]): RuleMap =
    rules.map(rule => (
      rule.left,
      s"${rule.left(0)}${rule.right(0)}",
    )).toMap

  @annotation.tailrec
  def step(rulesMap: RuleMap)(polymer: String)(n: Int): String =
    if (n <= 0) polymer else {
      val next = (
        polymer.sliding(2)
        .flatMap(x => rulesMap.getOrElse(x, x(0).toString))
        ++ List(polymer.last)
      )
      step(rulesMap)(next.mkString)(n-1)
  }

  def getScore(value: String) = {
    val alphabet = value.toSet
    // Inefficient, but whatever
    val counts = alphabet.map(ch => value.count(_ == ch))
    counts.max - counts.min
  }

  def test(): Unit = {
    val testPolymer = "NNCB"
    val testRules = makeRules(List(
      "CH -> B",
      "HH -> N",
      "CB -> H",
      "NH -> C",
      "HB -> C",
      "HC -> B",
      "HN -> C",
      "NN -> C",
      "BH -> H",
      "NC -> B",
      "NB -> B",
      "BN -> B",
      "BB -> N",
      "BC -> B",
      "CC -> N",
      "CN -> C",
    ).map(Rule.parse))
    val test = step(testRules)(testPolymer)(_)
    assert(test(1) == "NCNBCHB")
    assert(test(2) == "NBCCNBBBCBHCB")
    assert(test(3) == "NBBBCNCCNBBNBNBBCHBHHBCHB")
    assert(test(4) == "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")
    assert(getScore(test(10)) == 1588)
  }

  def main(args: Array[String]): Unit = {
    test()

    val lines = readLines("day_14_input.txt")
    val rulesMap = makeRules(lines.drop(2).map(Rule.parse))
    val polymer = lines.head

    val step10 = step(rulesMap)(polymer)(10)
    val score = getScore(step10)
    checkAnswer(14, 1, score)
  }
}
