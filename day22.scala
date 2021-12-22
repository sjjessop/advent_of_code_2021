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

  def main(args: Array[String]): Unit = {
    val program = readLines("day_22_input.txt").map(Instruction.parse)
    val onCount = run(program.map(_.trim(50)))
    checkAnswer(22, 1, onCount)
  }
}
