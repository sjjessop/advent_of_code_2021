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

  def expand(rulesMap: RuleMap)(pair: String): List[String] = {
    assert(pair.size == 2)
    rulesMap.get(pair) match {
      case None => List(pair)
      case Some(replacement) => List(replacement, s"${replacement(1)}${pair(1)}")
    }
  }

  def combine(pairs: Seq[(String, Long)]): Map[String, Long] = {
    pairs.groupBy(p => p._1).view.mapValues(vals => vals.map(_._2).sum).toMap
  }

  sealed case class Polymer(pairs: Map[String, Long], first: Char, last: Char) {
    @annotation.tailrec
    def step(rulesMap: RuleMap)(n: Int): Polymer = {
      if (n <= 0) this else {
        val next = pairs.toSeq.flatMap{case (value, count) =>
          expand(rulesMap)(value).map((_, count))
        }
        Polymer(combine(next), first, last).step(rulesMap)(n-1)
      }
    }
    def score: Long = {
      val alphabet = pairs.map(_._1).flatten.toSet.toList
      val pairList = pairs.toList
      def chScore(ch: Char) = pairList.map{case (p, count) => p.count(_ == ch) * count}.sum
      val counts: Map[Char, Long] = alphabet.map(ch => (ch, chScore(ch))).toMap
      // Now, each character occurrence in the polymer appears in two different pairs,
      // so all the scores are double what they should be, except for the first and last
      // chars, which didn't get double-counted.
      val realCounts = (counts.view
        .map{case (ch, count) => (ch, if (ch == first) count + 1 else count)}
        .map{case (ch, count) => (ch, if (ch == last) count + 1 else count)}
        .map{case (ch, count) => (ch, count / 2)}
     )
      val rawCounts = realCounts.map(_._2)
      rawCounts.max - rawCounts.min
    }
  }

  object Polymer {
    def fromString(value: String) = {
      Polymer(combine(value.sliding(2).map(p => (p, 1L)).toSeq), value.head, value.last)
    }
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

    assert(Polymer.fromString("NCNBCHB").score == getScore("NCNBCHB"))
    val step1 = Polymer.fromString("NCNBCHB").step(testRules)(1)
    assert(step1.pairs == Polymer.fromString("NBCCNBBBCBHCB").pairs)
    assert(step1.score == getScore("NBCCNBBBCBHCB"))
  }

  def main(args: Array[String]): Unit = {
    test()

    val lines = readLines("day_14_input.txt")
    val rulesMap = makeRules(lines.drop(2).map(Rule.parse))
    val polymer = lines.head

    val step10 = step(rulesMap)(polymer)(10)
    val score = getScore(step10)
    checkAnswer(14, 1, score)

    // Obviously not
    // val step40 = step(rulesMap)(polymer)(40)
    assert(Polymer.fromString(polymer).step(rulesMap)(10).score == score)
    val score40 = Polymer.fromString(polymer).step(rulesMap)(40).score
    checkAnswer(14, 2, score40)
  }
}
