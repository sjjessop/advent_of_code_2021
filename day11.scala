object day11 {

  import collection.MapView

  import day01.{readLines, checkAnswer}
  import day05.{Pos, Direction, up, down, left, right}

  def neighbours8(pos: Pos): List[Pos] = List(
    pos + right,
    pos + left,
    pos + up,
    pos + down,
    pos + up + right,
    pos + up + left,
    pos + down + right,
    pos + down + left,
  )

  def bumpNeighbours(energy: MapView[Pos, Int], flashing: Pos): MapView[Pos, Int] = {
    val near = neighbours8(flashing).toSet
    energy.map{ case (pos, value) => {
      if (near.contains(pos)) (pos, value + 1) else (pos, value)
   }}.toMap.view
  }

  def bumpAllNeighbours(energy: MapView[Pos, Int], flashing: Iterable[Pos]): MapView[Pos, Int] = {
    flashing.foldLeft(energy)((e, p) => bumpNeighbours(e, p))
  }

  @annotation.tailrec
  def getFlashes(energy: MapView[Pos, Int], flashed: Set[Pos] = Set.empty): (Int, MapView[Pos, Int]) = {
    val flashing = energy.filter(_._2 > 9).map(_._1).toSet diff flashed
    if (flashing.size == 0) (flashed.size, energy) else getFlashes(bumpAllNeighbours(energy, flashing), flashed union flashing)
  }

  class Cavern(val energy: Map[Pos, Int]) {
    def step: (Int, Cavern) = {
      val incremented = energy.view.mapValues(_ + 1)
      val (count, newEnergy) = getFlashes(incremented)
      (count, new Cavern(newEnergy.mapValues(x => if (x > 9) 0 else x).toMap))
    }
  }

  object Cavern {
    def parse(rows: List[String]): Cavern = new Cavern(
      rows
      .zip(0 until rows.length)
      .flatMap({ case (row, rowIdx) =>
        for ((energyValue, colIdx) <- row.zip(0 until row.length)) yield (Pos(rowIdx, colIdx), energyValue.toString.toInt)
      })
      .toMap
    )
  }

  def main(args: Array[String]): Unit = {
    val cave = Cavern.parse(readLines("day_11_input.txt"))
    val total = Iterator.unfold(cave)(c => Some(c.step)).take(100).sum
    checkAnswer(11, 1, total)
  }
}
