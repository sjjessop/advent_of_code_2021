object day23 {

  import collection.mutable

  import day01.{readLines, checkAnswer}

  // This is Dijkstra again.
  // There are 19 spaces in the map, 4 of which are uninhabitable by anything,
  // and 4 pairs of indistinguishable amphipods, so 15!/9!/2**4 = 16_216_200
  // game states, many of which are unreachable from any given start point.
  // That should be manageable provided the objects are kept small.
  //
  // Rather than another custom Dijkstra algorithm, let's write a generic one.

  def queueOrder[T](p: (T, Int)) = -p._2

  def dijkstra[V](start: V, end: V)(getNeighbours: V => Iterable[(V, Int)]): (Int, List[V]) = {
    // If we made this a map and returned it, we'd be calculating the shortest
    // distances to all nodes closer than "end". If we stored the best path as
    // well as the distances then we could report that too.
    val visited = mutable.Set.empty[V]
    val unvisited = mutable.Map(start -> 0).withDefaultValue(Int.MaxValue)
    val q = mutable.PriorityQueue[(V, Int)]()(Ordering.by(queueOrder))
    val bestTrack: mutable.Map[V, List[V]] = mutable.Map.empty.withDefaultValue(List(start))
    unvisited.foreach(p => q.enqueue(p))

    @annotation.tailrec
    def loop(): (Int, List[V]) = {
      val (nextNode, score) = q.dequeue()
      // if ((score % 1000) == 100) println(s"visited ${visited.size} unvisited ${unvisited.size} score ${score} node ${nextNode}")
      if (nextNode == end) (score, bestTrack(nextNode))
      else if (visited.contains(nextNode)) loop()
      else {
        // println(s"nextNode ${nextNode}")
        val unvisitedNeighbours = getNeighbours(nextNode).filter(p => !visited.contains(p._1))
        unvisitedNeighbours.foreach{
          case (v, weight) => {
            // println(s"neighbour ${v}")
            val nScore = score + weight
            if (nScore < unvisited(v)) {
              // This might queue up a dupe, but we'll process it before the
              // original (since it's better). When we process the original
              // we'll skip it because it's in "visted". This means we don't
              // need a queue that supports decreaseKey or similar.
              q.enqueue(v -> nScore)
              unvisited(v) = nScore
              bestTrack(v) = v::bestTrack(nextNode)
            }
          }
        }
        unvisited -= nextNode
        visited += nextNode
        loop()
      }
    }
    loop()
  }

  def charWeight(ch: Char): Int = ch match {
    case 'A' => 1
    case 'B' => 10
    case 'C' => 100
    case 'D' => 1000
  }

  val roomNames = List('A', 'B', 'C', 'D')

  // Compact representation of a game state as a string:
  // number the positions of the map as follows:
  //
  // #############
  // #89..etc....# to 18
  // ###1#3#5#7###
  //   #0#2#4#6#
  //   #########
  //
  // Then write the type of amphipod in each space as a letter or " " for empty
  // spaces. Concatenate these together and strip trailing spaces. So the
  // target state is always "AABBCCDD".
  //
  // We don't need to store information about which amphipods are "locked" in
  // the hallway, provided that we always list all possible hallway positions
  // as exits from the current state.
  //
  // Then as an optimisation: if we can move into a room we *do* move into the
  // room, since we have to go eventually and it's the same cost later as now.
  // It means we only have to consider one exit from that board position.
  //
  // Possible further optimisation: the doorway spaces are always empty, so we
  // don't need to store them in the packed representation, just in the hallway
  // expansion. Would save 4 chars per graph node. We could even pack down to
  // 2 bits per space, thus saving the whole thing in 30 bits of an Int.

  def atrium(pos: Int): Int = pos match {
    case 0 | 1 => 10
    case 2 | 3 => 12
    case 4 | 5 => 14
    case 6 | 7 => 16
    case _ => throw new Exception("bad")
  }

  def distance(src: Int, dest: Int): Int = {
    if (src >= 8 && dest >= 8) (src - dest).abs
    else if (src < 8) distance(atrium(src), dest) + (if (src % 2 == 0) 2 else 1)
    else /* (dest < 8) */ distance(atrium(dest), src) + (if (dest % 2 == 0) 2 else 1)
  }

  // Helper object to cache data and whatever. We'll dispose of it once we
  // know the exits from this board position, since we never need it again.
  case class Board(packed: String) {
    assert(packed.size >= 8)
    assert(packed.size <= 19)
    val hallway = (packed + "           ").substring(8)

    def exits: Iterable[(String, Int)] =
      pieceCanFinish match {
        case None => moveToHallway
        case some => some
      }

    def pieceCanFinish: Option[(String, Int)] =
      // The first one will do, then we'll find any others later.
      roomNames.view.flatMap(roomCanFinish).headOption

    def roomCanFinish(ch: Char): Option[(String, Int)] =
      for (dest <- roomOpen(ch); src <- findFinisher(ch)) yield move(src, dest)

    def roomLowerIdx(ch: Char) = (ch - 'A') * 2
    def roomUpperIdx(ch: Char) = (ch - 'A') * 2 + 1
    def roomLower(ch: Char) = packed(roomLowerIdx(ch))
    def roomUpper(ch: Char) = packed(roomUpperIdx(ch))

    // The position (if any) in a room where the next piece that belongs in
    // that room should move to.
    def roomOpen(ch: Char): Option[Int] = {
      if (roomUpper(ch) != ' ') None
      else roomLower(ch) match {
        case ' ' => Some(roomLowerIdx(ch))
        case `ch` => Some(roomUpperIdx(ch))
        case _ => None
      }
    }

    def isClearLeft(where: Int, door: Int): Boolean =
      hallway.substring(where + 1, door + 1).toSet == Set(' ')
    def isClearRight(where: Int, door: Int): Boolean =
      hallway.substring(door, where).toSet == Set(' ')
    // Params are hallway-based offsets, not overall position indexes.
    // The second parameter must be an atrium (because we assume it's clear)
    def isHallwayClear(where: Int, door: Int): Boolean =
      if (where < door) isClearLeft(where, door) else isClearRight(where, door)

    def findFinisher(ch: Char): Option[Int] = {
      // The empty space outside the room (atrium), in hallway coordinates.
      val target = (ch - 'A') * 2 + 2

      // Moves from the hallway into this room
      val fromLeft = (0 until target).view
      val fromRight = ((target + 1) to 11).view
      val fromHallway = {
        (fromLeft ++ fromRight)
          .filter(src => hallway(src) == ch)
          .filter(src => isHallwayClear(src, target))
          .map(_ + 8)
      }

      // Moves from another room into this room.
      val fromRoom = {
        val thisRoom = Set(roomLowerIdx(ch), roomUpperIdx(ch))
        // Positions in a room (not the hallway)...
        packed.view.take(8).zipWithIndex
          // ...containing a piece of the correct type
          .filter(_._1 == ch)
          .map(_._2)
          // ...in a different room, not this room.
          .filter(src => !thisRoom.contains(src))
          // ...with a clear road from this doorway to the target doorway
          .filter(src => isHallwayClear(atrium(src) - 8, target))
      }

      (fromHallway ++ fromRoom).headOption
    }

    def move(src: Int, dest: Int): (String, Int) = {
      val srcChar = packed(src)
      val nextBoard = {
        val mutableBuilder = new StringBuilder(packed)
        mutableBuilder.append("           ")
        mutableBuilder.setCharAt(src, mutableBuilder(dest))
        mutableBuilder.setCharAt(dest, srcChar)
        mutableBuilder.toString.reverse.dropWhile(_ == ' ').reverse
      }
      val cost = distance(src, dest) * charWeight(srcChar)
      (nextBoard, cost)
    }

    def moveToHallway: List[(String, Int)] = {
      // Can't say what order we should do the moves to hallway, since any
      // move makes some other move impossible. So, check them all.
      def movable(room: Char): List[Int] = {
        val upperIdx = roomUpperIdx(room)
        val lowerIdx = roomLowerIdx(room)
        val upperChar = packed(upperIdx)
        val lowerChar = packed(lowerIdx)
        if (upperChar != ' ' && upperChar != room) List(upperIdx)
        else if (upperChar != ' ' && lowerChar != room) List(upperIdx)
        else if (upperChar == ' ' && lowerChar != ' ' && lowerChar != room) List(lowerIdx)
        else List()
      }
      for (
        room <- roomNames;
        src <- movable(room);
        dest <- List(0, 1, 3, 5, 7, 9, 10).filter(dest => hallway(dest) == ' ').filter(dest => {
          val roomAtrium = atrium(roomLowerIdx(room)) - 8
          isHallwayClear(dest, roomAtrium)
        })
      ) yield move(src, dest + 8)
    }
  }

  def readFile(filename: String): String = {
    val lines = readLines(filename)
    val lower = lines(3)
    val upper = lines(2)
    List(lower(3), upper(3), lower(5), upper(5), lower(7), upper(7), lower(9), upper(9)).mkString
  }

  def prettyPrint(in: String): Unit = {
    val x = in.map(ch => if (ch == ' ') '.' else ch)
    println(x)
    println(x.substring(8))
    println(s"  ${x(1)}#${x(3)}#${x(5)}#${x(7)}")
    println(s"  ${x(0)}#${x(2)}#${x(4)}#${x(6)}")
  }

  def doTest(start: String, next: String): Unit = {
    val nextMoves = Board(start).exits.map(_._1).toList
    assert(nextMoves.contains(next))
  }

  def test(): Unit = {
    def grow(value: String) = List(value + "a" -> 1, value + "aaaa" -> 2)
    assert(dijkstra("", "aaaaaaaaaaaa")(grow)._1 == 6) // not 12

    // Stupidly easy to solve
    assert(solve("A BBCCDDA") == 3)
    assert(solve("  BBCCDDAA") == 6)
    assert(solve("AAB CCDDB") == 50)
    assert(solve("A BACCDDB") == 4 + 50)
    assert(getExits("DDAABBCC").size == 28)

    // One Bronze amphipod moves into the hallway, taking 4 steps and using 40 energy:
    doTest("ABDCCBAD", "ABDCC AD   B")
    // The only Copper amphipod not in its side room moves there, taking 4 steps and using 400 energy:
    doTest("ABDCC AD   B", "ABD CCAD   B")
    // A Desert amphipod moves out of the way, taking 3 steps and using 3000 energy, and then the Bronze amphipod takes its place, taking 3 steps and using 30 energy:
    doTest("ABD CCAD   B", "AB  CCAD   B D")
    doTest("AB  CCAD   B D", "ABB CCAD     D")
    // The leftmost Bronze amphipod moves to its room using 40 energy:
    doTest("ABB CCAD     D", "A BBCCAD     D")
    // Both amphipods in the rightmost room move into the hallway, using 2003 energy in total:
    doTest("A BBCCAD     D", "A BBCCA      D D")
    doTest("A BBCCA      D D", "A BBCC       D D A")
    // Both Desert amphipods move into the rightmost room using 7000 energy:
    doTest("A BBCC       D D A", "A BBCCD      D   A")
    doTest("A BBCCD      D   A", "A BBCCDD         A")
    // Finally, the last Amber amphipod moves into its room, using 8 energy:
    doTest("A BBCCDD         A", "AABBCCDD")
    assert(solve("ABDCCBAD") == 12521)

    // Regression test for bug where spaces were trimmed off the front
    val nextMoves = getExits("D CABCB D  A")
    assert(nextMoves.count(_._1 == "CABCB DD A") == 0)
  }

  def getExits(pos: String) = Board(pos).exits
  def solve(start: String) = dijkstra(start, "AABBCCDD")(getExits)._1

  def main(args: Array[String]): Unit = {
    test()

    val problem = readFile("day_23_input.txt")
    val result = solve(problem)
    checkAnswer(23, 1, result)
  }
}
