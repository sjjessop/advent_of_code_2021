object day15 {

  import collection.mutable

  import day01.{readLines, checkAnswer}
  import day05.{Pos, Direction, up, down, left, right}

  def neighbours4(pos: Pos) = Set(pos + up, pos + down, pos + left, pos + right)

  def getCostMap(lines: List[String]): Map[Pos, Int] = {
    val costs = (lines.map(_.toSeq.map(_.toString.toInt)))
    assert(costs.map(_.length).min == costs.map(_.length).max)
    (for(
      (row, y) <- costs.zipWithIndex;
      (value, x) <- row.zipWithIndex
    ) yield Pos(x, y) -> value).toMap
  }

  def merged[K, V](left: Map[K, V], right: Map[K, V])(func: List[V] => V): Map[K, V] = {
    (left.keys ++ right.keys).map(k => (k, func(List(left.get(k), right.get(k)).flatten))).toMap
  }

  @annotation.tailrec
  def dijkstraHelper(costMap: Map[Pos, Int], start: Pos, end: Pos, q: mutable.PriorityQueue[(Pos, Int)], unvisited: mutable.Map[Pos, Int]): Int = {
    // println(unvisited.size)
    val (nextNode, score) = q.dequeue()
    if (nextNode == end) score
    else {
      val inBounds = costMap.contains _
      val unvisitedNeighbours = (neighbours4(nextNode) intersect unvisited.keySet).filter(inBounds)
      unvisitedNeighbours.foreach(pos => {
        val nScore = score + costMap(pos)
        if (nScore < unvisited(pos)) {
          // This might queue up a dupe, but we'll process it before the
          // original (since it's better). Then when we process the original
          // we'll compute worse scores all around it. This means we don't need
          // a queue that supports decreaseKey or similar.
          q.enqueue((pos, nScore))
          unvisited(pos) = nScore
        }
      })
      unvisited -= nextNode
      dijkstraHelper(costMap, start, end, q, unvisited)
    }
  }

  def queueOrder(p: (Pos, Int)) = -p._2

  def dijkstra(costMap: Map[Pos, Int], start: Pos, end: Pos): Int = {
    val bestMap: Map[Pos, Int] = (costMap.view.mapValues(x => Int.MaxValue) ++ List(start -> 0)).toMap
    val q = mutable.PriorityQueue[(Pos, Int)]()(Ordering.by(queueOrder))
    bestMap.foreach(p => q.enqueue(p))
    dijkstraHelper(costMap, start, end, q, bestMap.to(mutable.Map))
  }

  def translate[V](tile: Map[Pos, V], d: Direction): Map[Pos, V] = {
    tile.view.map{ case (k, v) => (k + d, v) }.toMap
  }

  def main(args: Array[String]): Unit = {
    val lines = readLines("day_15_input.txt")
    val costMap = getCostMap(lines)
    val start = Pos(0, 0)
    val end = Pos(lines.head.size - 1, lines.size - 1)
    val ltr = dijkstra(costMap, start, end)
    checkAnswer(15, 1, ltr)

    def incr(y: Int)(x: Int): Int = ((x + y - 1) % 9) + 1

    val bigUp = up * lines.size
    val bigRight = right * lines.head.size
    val moves = for(x <- 0 to 4; y <- 0 to 4) yield bigUp * y + bigRight * x
    val bigMaps = moves.map(dir => translate(costMap, dir).view.mapValues(incr(dir.x + dir.y)).toMap)
    val bigCostMap = bigMaps.reduceLeft(_ ++ _)
    val bigEnd = Pos(lines.head.size * 5 - 1, lines.size * 5 - 1)
    val bigLtr = dijkstra(bigCostMap, start, bigEnd)
    checkAnswer(15, 2, bigLtr)
  }
}
