object day15 {

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
  def dijkstra(costMap: Map[Pos, Int], start: Pos, end: Pos, bestMap: Map[Pos, Int], visited: Set[Pos] = Set()): Int = {
    // println(visited.size)
    if (visited.contains(end)) bestMap(end)
    else {
      val nextNode = (costMap.keys.toSet diff visited).minBy(bestMap)
      val inBounds = costMap.keys.toSet.contains _
      val unvisitedNeighbours = (neighbours4(nextNode) diff visited).filter(inBounds)
      val routes = unvisitedNeighbours.map(pos => pos -> (bestMap(nextNode) + costMap(pos)))
      val newBest = merged(bestMap, routes.toMap)(_.min)
      // println(newBest.filter{case (k, v) => v != Int.MaxValue})
      dijkstra(costMap, start, end, newBest, visited union Set(nextNode))
    }
  }


  def main(args: Array[String]): Unit = {
    val lines = readLines("day_15_input.txt")
    val costMap = getCostMap(lines)
    val start = Pos(0, 0)
    val bestMap: Map[Pos, Int] = (costMap.view.mapValues(x => Int.MaxValue) ++ List(start -> 0)).toMap
    val end = Pos(lines.head.size - 1, lines.size - 1)
    val ltr = dijkstra(costMap, start, end, bestMap)
    checkAnswer(15, 1, ltr)
  }
}
