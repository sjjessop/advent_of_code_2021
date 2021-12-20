object day18 {

  import collection.mutable

  import day01.{readLines, checkAnswer}

  trait Turn {}
  case object left extends Turn {}
  case object right extends Turn {}

  // The *end* of the list is the *root* of the tree
  // Addresses will never get longer than 5 turns.
  //
  // We do a lot of address comparisons (which need to reverse the lists), and
  // relatively fewer modifications and magnitude calculations (which operate
  // at the head of the list). So you'd think it would be more efficient to
  // represent in the opposite order. But it doesn't seem to affect runtime
  // much either way, so I'll stick with what I did first.
  type Address = List[Turn]

  object Address {
    val turns = List(left, right)
    def lt(a: Address, b: Address): Boolean = lexical(a.reverse, b.reverse)

    @annotation.tailrec
    def lexical(a: Address, b: Address): Boolean = {
      if (b == Nil) false
      else if (a == Nil) true
      else if (a.head == left && b.head == right) true
      else if (a.head == right && b.head == left) false
      else lexical(a.tail, b.tail)
    }

    // All addresses that can occur in an initial sum (before reduction)
    def allUnsorted: List[Address] = {
      val ones = for (a <- turns) yield List(a)
      val twos = for(a <- turns; b <- turns) yield List(b, a)
      val threes = for(a <- turns; b <- turns; c <- turns) yield List(c, b, a)
      val fours = for(a <- turns; b <- turns; c <- turns; d <- turns) yield List(d, c, b, a)
      val fives = for(a <- turns; b <- turns; c <- turns; d <- turns; e <- turns) yield List(e, d, c, b, a)
      ones ++ twos ++ threes ++ fours ++ fives
    }

    // All addresses in lexicographical order
    val all: List[Address] = allUnsorted.sortWith(lt)

    // Addresses too deeply nested to live (we find the leftmost, then deduce the right side of the pair later)
    val tooDeep: List[Address] = {
      for(a <- turns; b <- turns; c <- turns; d <- turns) yield List(left, d, c, b, a)
    }
  }

  // Represent the nested pairs of a Snailfish number by a map from sequences
  // of turns to the values stored there. Of interest is that a correctly-
  // formed Snail never has two addresses one of which is a prefix of another.
  sealed case class Snail(content: Map[Address, Int]) {
    // @annotation.tailrec
    def pretty(path: Address = Nil): String =
      content.get(path).map(_.toString).getOrElse(s"[${pretty(left::path)},${pretty(right::path)}]")

    // Can only go 4 deep, so won't overflow Int
    // @annotation.tailrec
    def magnitude(path: Address = Nil): Int =
      content.get(path).getOrElse(3 * magnitude(left::path) + 2 * magnitude(right::path))

    def +(other: Snail) = Snail((
      content.view.map{ case (k, v) => (k :+ left, v) } ++
      other.content.view.map{ case (k, v) => (k :+ right, v)}
    ).toMap).reduced

    @annotation.tailrec
    def reduced: Snail = {
      if (mustExplode.isDefined) explode().reduced
      else if (mustSplit.isDefined) split().reduced
      else this
    }

    lazy val mustExplode: Option[Address] = Address.tooDeep.view.filter(path => content.contains(path)).headOption

    def explode(): Snail = {
      val leftPath = mustExplode.get
      val rightPath = right::(leftPath.tail)
      val leftValue = content(leftPath)
      val rightValue = content(rightPath)
      val paths = Address.all.filter(path => content.contains(path))
      val leftMovesTo = paths.filter(path => Address.lt(path, leftPath)).lastOption
      val rightMovesTo = paths.view.filter(path => Address.lt(rightPath, path)).headOption

      val mutableContent = content.to(mutable.Map)
      def incr(where: Option[Address], by: Int) =
        where.foreach(path => mutableContent(path) = mutableContent(path) + by)
      incr(leftMovesTo, leftValue)
      incr(rightMovesTo, rightValue)
      mutableContent -= leftPath
      mutableContent -= rightPath
      mutableContent(leftPath.tail) = 0
      Snail(mutableContent.toMap)
    }

    lazy val mustSplit: Option[Address] = Address.all.view.filter(path => content.getOrElse(path, 0) >= 10).headOption

    def split(): Snail = {
      val oldPath = mustSplit.get
      val oldValue = content(oldPath)
      val leftValue = oldValue / 2
      val rightValue = (oldValue + 1) / 2

      val mutableContent = content.to(mutable.Map)
      mutableContent -= oldPath
      mutableContent(left::oldPath) = leftValue
      mutableContent(right::oldPath) = rightValue
      Snail(mutableContent.toMap)
    }
  }

  sealed case class Builder(content: Map[Address, Int], at: Address) {
    def add(ch: Char) = ch match {
      case '[' => Builder(content, left::at)
      case ',' => Builder(content, right::(at.tail))
      case ']' => Builder(content, at.tail)
      case digit => {
        assert(!content.contains(at))
        Builder(content + (at -> (digit - '0')), at)
      }
    }

    def toSnail = {
      assert(at == Nil)
      Snail(content)
    }
  }

  object Builder {
    // This is not a well-formed number, but we use it to start building.
    val empty = Builder(Map.empty, Nil)
  }

  object Snail {
    def parse(value: String): Snail = {
      value.foldLeft(Builder.empty)(_.add(_)).toSnail
    }
  }

  def test(): Unit = {
    def testParse(value: String) = {
      val result = Snail.parse(value).pretty()
      assert(value == result)
    }
    testParse("[1,2]")
    testParse("[[1,2],3]")
    testParse("[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]")
    assert(Snail.parse("[1,2]") + Snail.parse("[[3,4],5]") == Snail.parse("[[1,2],[[3,4],5]]"))
    assert(Snail.parse("[[[[[9,8],1],2],3],4]").explode() == Snail.parse("[[[[0,9],2],3],4]"))
    assert(Snail.parse("[7,[6,[5,[4,[3,2]]]]]").explode() == Snail.parse("[7,[6,[5,[7,0]]]]"))
    assert(Snail.parse("[[6,[5,[4,[3,2]]]],1]").explode() == Snail.parse("[[6,[5,[7,0]]],3]"))
    assert(Snail.parse("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]").explode() == Snail.parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))
    assert(Snail.parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]").explode() == Snail.parse("[[3,[2,[8,0]]],[9,[5,[7,0]]]]"))
    assert(Snail.parse("[[[[4,3],4],4],[7,[[8,4],9]]]") + Snail.parse("[1,1]") == Snail.parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))

    val test1Data = List(
      "[1,1]",
      "[2,2]",
      "[3,3]",
      "[4,4]",
      "[5,5]",
      "[6,6]",
    )
    assert(test1Data.take(4).map(Snail.parse).reduceLeft(_+_) == Snail.parse("[[[[1,1],[2,2]],[3,3]],[4,4]]"))
    assert(test1Data.take(5).map(Snail.parse).reduceLeft(_+_) == Snail.parse("[[[[3,0],[5,3]],[4,4]],[5,5]]"))
    assert(test1Data.map(Snail.parse).reduceLeft(_+_) == Snail.parse("[[[[5,0],[7,4]],[5,5]],[6,6]]"))

    val test2Data = List(
      "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
      "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
      "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
      "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
      "[7,[5,[[3,8],[1,4]]]]",
      "[[2,[2,2]],[8,[8,1]]]",
      "[2,9]",
      "[1,[[[9,3],9],[[9,0],[0,7]]]]",
      "[[[5,[7,4]],7],1]",
      "[[[[4,2],2],6],[8,7]]",
    )
    assert(test2Data.take(2).map(Snail.parse).reduceLeft(_+_) == Snail.parse("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"))
    assert(test2Data.map(Snail.parse).reduceLeft(_+_) == Snail.parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))

    assert(Snail.parse("[9,1]").magnitude() == 29)
    assert(Snail.parse("[1,9]").magnitude() == 21)
    assert(Snail.parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").magnitude() == 3488)
  }

  def main(args: Array[String]): Unit = {
    test()

    val nums = readLines("day_18_input.txt").map(Snail.parse)
    val result = nums.reduceLeft(_+_).magnitude()
    checkAnswer(18, 1, result)

    val pairs = (for(a <- nums; b <- nums) yield (a, b)).filter(p => p._1 != p._2)
    val result2 = pairs.map(p => p._1 + p._2).map(_.magnitude()).max
    checkAnswer(18, 2, result2)
  }
}
