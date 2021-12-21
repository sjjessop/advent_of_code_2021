object day19 {

  import day01.{readLines, checkAnswer}

  sealed case class Pos(x: Int, y: Int, z: Int) {
    lazy val coords = List(x,y,z)
    lazy val negs = coords.map(_.sign == -1).count(_ == true)
    def +(other: Direction) = Pos(x + other.x, y + other.y, z + other.z)
    def -(other: Pos) = Direction(x - other.x, y - other.y, z - other.z)
  }

  object Pos {
    def parse(value: String) = {
      val parts = value.split(",").map(_.toInt)
      if (parts.size != 3) throw new Exception("bad")
      Pos(parts(0), parts(1), parts(2))
    }
  }

  sealed case class Direction(x: Int, y: Int, z: Int) {}

  type Operation = Pos => Pos

  val rotations: List[Operation] = List(
    // x and y can go anywhere, then figure out z by the left-hand rule
    // Or to put it another way, ignoring sign there are 6 permutations, then:
    // * if all axes stay put you need an even number of negative signs (4)
    // * if one axis stays put and two swap you need an odd number of - signs (3 * 4)
    // * if the three axes cycle you need an even number of - signs (2 * 4)
    // If I could be bothered with matrix operations, then it's the ones with
    // positive determinant we want.
    pos => Pos(pos.x, pos.y, pos.z),
    pos => Pos(pos.x, -pos.y, -pos.z),
    pos => Pos(-pos.x, pos.y, -pos.z),
    pos => Pos(-pos.x, -pos.y, pos.z),

    pos => Pos(pos.x, pos.z, -pos.y),
    pos => Pos(pos.x, -pos.z, pos.y),
    pos => Pos(-pos.x, pos.z, pos.y),
    pos => Pos(-pos.x, -pos.z, -pos.y),

    pos => Pos(pos.z, pos.y, -pos.x),
    pos => Pos(pos.z, -pos.y, pos.x),
    pos => Pos(-pos.z, pos.y, pos.x),
    pos => Pos(-pos.z, -pos.y, -pos.x),

    pos => Pos(pos.y, pos.x, -pos.z),
    pos => Pos(pos.y, -pos.x, pos.z),
    pos => Pos(-pos.y, pos.x, pos.z),
    pos => Pos(-pos.y, -pos.x, -pos.z),

    pos => Pos(pos.y, pos.z, pos.x),
    pos => Pos(pos.y, -pos.z, -pos.x),
    pos => Pos(-pos.y, pos.z, -pos.x),
    pos => Pos(-pos.y, -pos.z, pos.x),

    pos => Pos(pos.z, pos.x, pos.y),
    pos => Pos(pos.z, -pos.x, -pos.y),
    pos => Pos(-pos.z, pos.x, -pos.y),
    pos => Pos(-pos.z, -pos.x, pos.y),
  )

  def testRotations(): Unit = {
    // Check we haven't duped anything
    assert(rotations.size == 24)
    val outputs = rotations.map(r => r(Pos(1,2,3)))
    assert(outputs.toSet.size == 24)
    assert(outputs.filter(p => Set(p.x.abs, p.y.abs, p.z.abs) == Set(1,2,3)).size == 24)
    // Check the three rules stated
    // Identity permutation
    assert(outputs.filter(_.coords.map(_.abs) == List(1,2,3)).filter(_.negs % 2 == 0).size == 4)
    // 2-cycles
    assert(outputs.filter(_.coords.map(_.abs) == List(1,3,2)).filter(_.negs % 2 != 0).size == 4)
    assert(outputs.filter(_.coords.map(_.abs) == List(3,2,1)).filter(_.negs % 2 != 0).size == 4)
    assert(outputs.filter(_.coords.map(_.abs) == List(2,1,3)).filter(_.negs % 2 != 0).size == 4)
    // 3-cycles
    assert(outputs.filter(_.coords.map(_.abs) == List(2,3,1)).filter(_.negs % 2 == 0).size == 4)
    assert(outputs.filter(_.coords.map(_.abs) == List(3,1,2)).filter(_.negs % 2 == 0).size == 4)

    val test1_1 = Set(
      Pos(-1,-1,1),
      Pos(-2,-2,2),
      Pos(-3,-3,3),
      Pos(-2,-3,1),
      Pos(5,6,-4),
      Pos(8,0,7),
    )
    val test1_2 = Set(
      Pos(1,-1,1),
      Pos(2,-2,2),
      Pos(3,-3,3),
      Pos(2,-1,3),
      Pos(-5,4,-6),
      Pos(-8,-7,0),
    )
    val test1_5 = Set(
      Pos(1,1,1),
      Pos(2,2,2),
      Pos(3,3,3),
      Pos(3,1,2),
      Pos(-6,-4,-5),
      Pos(0,7,-8),
    )
    assert(rotations.count(r => test1_2.map(r) == test1_1) == 1)
    assert(rotations.count(r => test1_5.map(r) == test1_1) == 1)
  }

  def translate(dir: Direction): Operation = _ + dir

  def getTranslation(a: Set[Pos], b: Set[Pos], threshold: Int = 12): Option[Operation] = {
    // We don't need to test all targets, because of n matches, they can't
    // all be in the first n-1. So we'll find one in the rest.
    val ops = for (target <- a.drop(threshold - 1).view; attempt <- b.view)
      yield translate(target - attempt)
    ops.filter(op => (b.map(op) intersect a).size >= threshold).headOption
  }

  def testTranslations(): Unit = {
    val test0_1 = Set(
      Pos(0,2,0),
      Pos(4,1,0),
      Pos(3,3,0),
    )
    val test0_2 = Set(
      Pos(-1,-1,0),
      Pos(-5,0,0),
      Pos(-2,1,0),
    )
    assert((test0_2.map(translate(Direction(5,2,0))) intersect test0_1).size >= 3)
    val test0_overlap = getTranslation(test0_1, test0_2, 3)
    assert((test0_2.map(test0_overlap.get) intersect test0_1).size >= 3)
  }

  case class Scanner(name: String, beacons: Set[Pos]) {
    def align(other: Scanner): Option[Operation] =
      // We're looking for a rotation followed by a translation.
      rotations.view.flatMap(
        r => getTranslation(beacons, other.apply(r).beacons).map(_ compose r)
      ).headOption
    def apply(op: Operation) = {
      Scanner(name + "fixed", beacons.map(op))
    }
  }

  object Scanner {
    def parse(report: String) = {
      val lines = report.split('\n').toList
      val beacons = lines.tail.map(Pos.parse)
      Scanner(lines.head, beacons.toSet)
    }
  }

  def readFile(filename: String): Seq[Scanner] =
    readLines(filename).mkString("\n").split("\n\n").map(Scanner.parse).toSeq

  def testFull() = {
    val scanners = readFile("day_19_input_test.txt")
    val op_1To0 = scanners(0).align(scanners(1)).get
    assert((scanners(0).beacons intersect scanners(1).apply(op_1To0).beacons) == Set(
      Pos(-618,-824,-621),
      Pos(-537,-823,-458),
      Pos(-447,-329,318),
      Pos(404,-588,-901),
      Pos(544,-627,-890),
      Pos(528,-643,409),
      Pos(-661,-816,-575),
      Pos(390,-675,-793),
      Pos(423,-701,434),
      Pos(-345,-311,381),
      Pos(459,-707,401),
      Pos(-485,-357,347),
    ))
    val op_4To1 = scanners(1).align(scanners(4)).get
    assert((scanners(1).apply(op_1To0).beacons intersect scanners(4).apply(op_1To0 compose op_4To1).beacons) == Set(
      Pos(459,-707,401),
      Pos(-739,-1745,668),
      Pos(-485,-357,347),
      Pos(432,-2009,850),
      Pos(528,-643,409),
      Pos(423,-701,434),
      Pos(-345,-311,381),
      Pos(408,-1815,803),
      Pos(534,-1912,768),
      Pos(-687,-1600,576),
      Pos(-447,-329,318),
      Pos(-635,-1737,486),
    ))
    assert(op_1To0(Pos(0,0,0)) == Pos(68,-1246,-43))
    assert(op_1To0(op_4To1(Pos(0,0,0))) == Pos(-20,-1133,1061))
  }

  // The original scanner together with the operation required to map it to scanner0
  type FixedScanner = (Scanner, Operation)
  def fix(fs: FixedScanner) = fs._1.apply(fs._2)

  @annotation.tailrec
  def alignAll(unaligned: Set[Scanner], next: Set[Scanner], aligned: Map[Scanner, Operation]): Map[Scanner, Operation] = {
    // println(s"${aligned.size} down, ${unaligned.size} to go")
    if (unaligned.isEmpty) aligned
    else if (next.isEmpty) throw new Exception("stuck")
    else {
      // This could stop after finding one match for each unaligned scanner,
      // but it inefficiently finds all overlapping pairs. But it's just about
      // fast enough as it is.
      val newMatches: Set[FixedScanner] = for (
        target <- next;
        toFix <- unaligned;
        op <- target.align(toFix)
      ) yield {
        // println(s"${toFix.name} matches ${target.name}")
        (toFix, aligned(target) compose op)
      }
      val nextNext = newMatches.map(_._1).toSet
      alignAll(
        unaligned=unaligned diff nextNext,
        next=nextNext,
        aligned=aligned ++ newMatches,
      )
    }
  }

  def main(args: Array[String]): Unit = {
    testRotations()
    testTranslations()
    testFull()

    val scanners = readFile("day_19_input.txt")
    val scanner0 = scanners.head

    val solution = alignAll(scanners.tail.toSet, Set(scanner0), Map(scanner0 -> identity))
    val allBeacons = solution.map(fix).flatMap(_.beacons).toSet
    val result = allBeacons.size
    checkAnswer(19, 1, result)
  }
}
