object day22 {

  import day01.{readLines, checkAnswer}

  sealed case class Pos(x: Int, y: Int, z: Int) {}

  case class Block(xMin: Int, xMax: Int, yMin: Int, yMax: Int, zMin: Int, zMax: Int) {
    def trim(n: Int) = Block(
      math.max(xMin, -n),
      math.min(xMax, n),
      math.max(yMin, -n),
      math.min(yMax, n),
      math.max(zMin, -n),
      math.min(zMax, n),
    )
    def poses = for (x <- xMin to xMax; y <- yMin to yMax; z <- zMin to zMax) yield Pos(x,y,z)
    def isEmpty = (xMax < xMin) || (yMax < yMin) || (zMax < zMin)
    def size: Long = if (isEmpty) 0L else (xMax - xMin + 1).toLong * (yMax - yMin + 1).toLong * (zMax - zMin + 1).toLong
    def intersect(other: Block) = Block(
      math.max(xMin, other.xMin),
      math.min(xMax, other.xMax),
      math.max(yMin, other.yMin),
      math.min(yMax, other.yMax),
      math.max(zMin, other.zMin),
      math.min(zMax, other.zMax),
    )
  }

  object Block {
    def parse(value: String): Block = {
      val words = value.split(",")
      assert(words.size == 3)
      def parseRange(range: String): (Int, Int) = {
        val parts = range.split("\\.\\.")
        assert(parts.size == 2)
        val nums = parts.map(_.toInt)
        (nums(0), nums(1))
      }
      assert(words(0)(0) == 'x')
      val (xMin, xMax) = parseRange(words(0).drop(2))
      assert(words(1)(0) == 'y')
      val (yMin, yMax) = parseRange(words(1).drop(2))
      assert(words(2)(0) == 'z')
      val (zMin, zMax) = parseRange(words(2).drop(2))
      Block(xMin, xMax, yMin, yMax, zMin, zMax)

    }
  }

  case class Instruction(block: Block, setValue: Int) {
    def trim(n: Int) = Instruction(block.trim(n), setValue)
    def lit: Long = block.size * setValue
  }

  object Instruction {
    def parse(value: String): Instruction = {
      val words = value.split(" ")
      assert(words.size == 2)
      val setValue = if (words(0) == "on") 1 else if (words(0) == "off") 0 else ???
      Instruction(Block.parse(words(1)), setValue)
    }
  }

  def run(program: Iterable[Instruction]) = {
    // I don't like the look of the big numbers in most of the instructions
    // from the input file, but here goes with the naive solution for 1M cubes.
    program.foldLeft(collection.View.empty[(Pos, Int)])((view, insn) => {
      // println(insn)
      view ++ insn.block.poses.map(pos => (pos -> insn.setValue))
    }).toMap.values.count(_ == 1)
  }

  def combine(left: Instruction, right: Instruction): List[Instruction] = {
    // We know the right instruction will be added into the total later (well,
    // if it's "off" we'll skip it for efficiency, but added in effect). Our
    // job is to account for everything except that.
    //
    // Therefore return a list containing this instruction, and another
    // instruction that reverses the effect of this instruction for the
    // intersection between the two.
    //
    // Then, when everything is put together the result will be that on the
    // intersection of the two instructions, the right takes effect, while on
    // the points only in one of the two, that one takes effect. Which is the
    // correct semantics for the instructions.
    //
    // The points of the intersection will be present in the list both in the
    // intersection and as part of each of "left" and "right", but will cancel
    // out in the total (Inclusion-Exclusion principle).
    val intersect = (
      List(left.block intersect right.block)
      // Optimisation - don't create empty blocks that will need processing later.
      .filter(!_.isEmpty)
      // Slight abuse of Instruction here: allowing negative values means we're
      // now treating it as something that is totalled, rather than something
      // where the last value takes precedence. But otherwise it's the same
      // type, so just use it.
      .map(b => Instruction(b, -1 * left.setValue))
    )
    left::intersect
  }

  def addInstruction(insns: List[Instruction], newInsn: Instruction): List[Instruction] = {
    // println(s"adding to ${insns.size}")
    val previous = insns.flatMap(insn => combine(insn, newInsn))
    newInsn.setValue match {
      case 1 => newInsn::previous
      // Optimisation - don't create 0 blocks that will need processing later.
      case 0 => previous
      // newInsn must be a parsed Instruction, not the fake -1s from combination.
      case _ => ???
    }
  }

  def combineAll(program: List[Instruction]) = {
    program.foldLeft(List.empty[Instruction])(addInstruction)
  }

  def runAll(program: List[Instruction]) = combineAll(program).map(_.lit).sum

  def test(program: List[Instruction], expected: Long): Unit = {
    val examples = List(
      "on x=10..12,y=10..12,z=10..12",
      "on x=11..13,y=11..13,z=11..13",
      "off x=9..11,y=9..11,z=9..11",
      "on x=10..10,y=10..10,z=10..10",
    ).map(Instruction.parse)
    assert((combine(examples(0), examples(1)) :+ examples(1)).map(_.lit) == List(27, -8, 27))
    assert(runAll(examples.take(2)) == 27 + 19)
    assert(runAll(examples.take(3)) == 27 + 19 - 8)
    assert(runAll(examples.take(4)) == 27 + 19 - 8 + 1)
    assert(runAll(program.map(_.trim(50)).filter(!_.block.isEmpty)) == expected)
  }

  def main(args: Array[String]): Unit = {
    val program = readLines("day_22_input.txt").map(Instruction.parse)
    val onCount = run(program.map(_.trim(50)))
    checkAnswer(22, 1, onCount)

    test(program, onCount)

    val result = runAll(program)
    checkAnswer(22, 2, result)
  }
}
