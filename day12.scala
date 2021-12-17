object day12 {

  import day01.{readLines, checkAnswer}

  sealed case class Cave(name: String, exits: List[String]) {
    def isBig: Boolean = name.head.isUpper
    def isSmall: Boolean = !isBig
  }

  case class Edge(a: String, b: String) {
    def members: List[String] = List(a, b)
  }

  object Edge {
    def parse(value: String): Edge = {
      value.split("-").toList match {
        case a::b::Nil => Edge(a, b)
        case _ => throw new Exception("bad")
      }
    }
  }

  case class Route(path: List[String]) {
    val at = path.head
    lazy val blocked: Set[String] = path.filter(_.head.isLower).toSet
    def advance(exits: List[String]): List[Route] = {
      if (at == "end") List(this) else {
        exits.filter(!blocked.contains(_)).map(x => Route(x::path))
      }
    }
  }

  val startRoute = Route("start"::Nil)

  sealed case class Graph(edges: List[Edge]) {
    lazy val caves: Map[String, Cave] = {
      val nodes = edges.flatMap(_.members).toSet
      // Not very efficient, but cave system is small
      def adjacent(name: String): List[String] = {
        val names = edges.filter(_.members.contains(name)).flatMap(_.members).toSet
        (names diff Set(name)).toList
      }
      nodes.map(name => (name, Cave(name, adjacent(name)))).toMap
    }

    @annotation.tailrec
    def routes(sofar: Set[Route] = Set(startRoute)): Set[Route] = {
      val advanced: Set[Route] = sofar.flatMap(route => route.advance(caves(route.at).exits))
      if ((advanced union sofar).size == sofar.size) advanced else routes(advanced)
    }
  }

  def solve(lines: List[String]) = {
    val system = Graph(lines.map(Edge.parse))
    system.routes().filter(_.at == "end").size
  }

  def test(): Unit = {
    assert(solve(List(
      "start-A",
      "start-b",
      "A-c",
      "A-b",
      "b-d",
      "A-end",
      "b-end",
    )) == 10)
  }

  def main(args: Array[String]): Unit = {
    test()

    val routeCount = solve(readLines("day_12_input.txt"))
    checkAnswer(12, 1, routeCount)
  }
}
