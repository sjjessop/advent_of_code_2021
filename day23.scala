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
  //
  // Part 2 will be similar, but 4 positions per room.

  // Helper object to cache data and whatever. We'll dispose of it once we
  // know the exits from this board position, since we never need it again.
  case class Board(packed: String, roomSize: Int = 2) {
    val hallwayOffset = 4 * roomSize
    assert(packed.size >= hallwayOffset)
    assert(packed.size <= hallwayOffset + 11)
    val hallway = (packed + "           ").substring(hallwayOffset)

    // The atrium (or doorway) to a room is the space in the hall just outside.
    def atrium(pos: Int): Int =
      if (pos < hallwayOffset) hallwayOffset + 2 + 2 * (pos / roomSize)
      else ???

    def distance(src: Int, dest: Int): Int = {
      if (src >= hallwayOffset && dest >= hallwayOffset) (src - dest).abs
      else if (src < hallwayOffset) distance(atrium(src), dest) + roomSize - (src % roomSize)
      else distance(atrium(dest), src) + roomSize - (dest % roomSize)
    }

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

    def roomLowerIdx(ch: Char) = (ch - 'A') * roomSize

    // The position (if any) in a room where the next piece that belongs in
    // that room should move to.
    def roomOpen(ch: Char): Option[Int] = {
      // A room is open if and only if it consists of 0 or more of the correct
      // piece followed one or more spaces
      val roomStart = roomLowerIdx(ch)
      val roomContent = packed.substring(roomStart, roomStart + roomSize)
      val roomSpace = roomContent.dropWhile(_ == ch)
      Option.when(roomSpace.toSet == Set(' '))(roomStart + roomSize - roomSpace.size)
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
          .map(_ + hallwayOffset)
      }

      // Moves from another room into this room.
      val fromRoom = {
        val roomStart = roomLowerIdx(ch)
        val thisRoom = (roomStart until roomStart + roomSize)
        def endOf(idx: Int) = idx + (roomSize - idx % roomSize)
        // Positions in a room (not the hallway)...
        packed.view.take(hallwayOffset).zipWithIndex
          // ...containing a piece of the correct type
          .filter(_._1 == ch)
          .map(_._2)
          // ...in a different room, not this room.
          .filter(src => !thisRoom.contains(src))
          // ...with a clear road from this doorway to the target doorway
          // (we've already established the road from the target doorway in roomOpen)
          .filter(src => isHallwayClear(atrium(src) - hallwayOffset, target))
          // ...and a clear road from the piece to this doorway
          .filter(src => ((src + 1) until endOf(src)).map(packed).filter(_ != ' ').isEmpty)
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
        // A room is movable if the top piece in it doesn't belong there,
        // or if a piece behind that needs to get out past it.
        val roomStart = roomLowerIdx(room)
        val roomContent = packed.substring(roomStart, roomStart + roomSize).trim()
        val roomEnd = roomStart + roomContent.size - 1
        if (roomContent.size == 0) List()
        else if (roomContent.last != room) List(roomEnd)
        else if (roomContent.dropWhile(_ != room) != roomContent) List(roomEnd)
        else List()
      }
      for (
        room <- roomNames;
        src <- movable(room);
        dest <- List(0, 1, 3, 5, 7, 9, 10).filter(dest => hallway(dest) == ' ').filter(dest => {
          val roomAtrium = atrium(roomLowerIdx(room)) - hallwayOffset
          // println(s"checking out room=${room} src=${src} dest=${dest} door=${roomAtrium} ok=${isHallwayClear(dest, roomAtrium)}")
          isHallwayClear(dest, roomAtrium)
        })
      ) yield move(src, dest + hallwayOffset)
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

  def prettyPrint4(in: String): Unit = {
    val x = in.map(ch => if (ch == ' ') '.' else ch)
    println(x)
    println(x.substring(16))
    println(s"  ${x(3)}#${x(7)}#${x(11)}#${x(15)}")
    println(s"  ${x(2)}#${x(6)}#${x(10)}#${x(14)}")
    println(s"  ${x(1)}#${x(5)}#${x(9)}#${x(13)}")
    println(s"  ${x(0)}#${x(4)}#${x(8)}#${x(12)}")
  }

  def doTest(start: String, next: String): Unit = {
    val nextMoves = Board(start).exits.map(_._1).toList
    assert(nextMoves.contains(next))
  }

  def doTest4(start: String, next: String): Unit = {
    val nextMoves = Board(start, 4).exits.map(_._1).toList
    assert(nextMoves.contains(next))
  }

  def test(): Unit = {
    def grow(value: String) = List(value + "a" -> 1, value + "aaaa" -> 2)
    assert(dijkstra("", "aaaaaaaaaaaa")(grow)._1 == 6) // not 12

    assert(Board("AABBCCDD").atrium(0) == 10)
    assert(Board("AABBCCDD").distance(0, 10) == 2)
    assert(Board("AABBCCDD").distance(8, 1) == 3)
    assert(Board("AAAABBBBCCCCDDDD", 4).atrium(0) == 18)
    assert(Board("AAAABBBBCCCCDDDD", 4).distance(0, 18) == 4)
    assert(Board("AAAABBBBCCCCDDDD", 4).distance(16, 1) == 5)

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

    doTest4("ADDBDBCCCABBACAD", "ADDBDBCCCABBACA           D")
    doTest4("ADDBDBCCCABBACA           D", "ADDBDBCCCABBAC  A         D")
    doTest4("ADDBDBCCCABBAC  A         D", "ADDBDBCCCAB AC  A        BD")
    doTest4("ADDBDBCCCAB AC  A        BD", "ADDBDBCCCA  AC  A      B BD")
    doTest4("ADDBDBCCCA  AC  A      B BD", "ADDBDBCCC   AC  AA     B BD")
    doTest4("ADDBDBCCC   AC  AA     B BD", "ADDBDBC CC  AC  AA     B BD")
    doTest4("ADDBDBC CC  AC  AA     B BD", "ADDBDB  CCC AC  AA     B BD")
    doTest4("ADDBDB  CCC AC  AA     B BD", "ADDBD   CCC AC  AA   B B BD")
    doTest4("ADDBD   CCC AC  AA   B B BD", "ADDB    CCC AC  AA D B B BD")
    doTest4("ADDB    CCC AC  AA D B B BD", "ADDBB   CCC AC  AA D   B BD")
    doTest4("ADDBB   CCC AC  AA D   B BD", "ADDBBB  CCC AC  AA D     BD")
    doTest4("ADDBBB  CCC AC  AA D     BD", "ADDBBBB CCC AC  AA D      D")
    doTest4("ADDBBBB CCC AC  AA D      D", "ADDBBBB CCCCA   AA D      D")
    doTest4("ADDBBBB CCCCA   AA D      D", "ADDBBBB CCCC    AA D     AD")
    doTest4("ADDBBBB CCCC    AA D     AD", "ADDBBBB CCCCD   AA       AD")
    doTest4("ADDBBBB CCCCD   AA       AD", "ADD BBBBCCCCD   AA       AD")
    doTest4("ADD BBBBCCCCD   AA       AD", "AD  BBBBCCCCDD  AA       AD")
    doTest4("AD  BBBBCCCCDD  AA       AD", "A   BBBBCCCCDDD AA       AD") // here a difference
    doTest4("A   BBBBCCCCDDD AA       AD", "AA  BBBBCCCCDDD A        AD") // here
    doTest4("AA  BBBBCCCCDDD A        AD", "AAA BBBBCCCCDDD          AD") // matches again
    doTest4("AAA BBBBCCCCDDD          AD", "AAAABBBBCCCCDDD           D")
    doTest4("AAAABBBBCCCCDDD           D", "AAAABBBBCCCCDDDD")
    assert(solve4("ADDBDBCCCABBACAD") == 44169)

    // Regression test another illegal move
    assert(Board("A   DBCCCAB ACA B    D D DB", 4).exits.count(_._1 == "A   DBCCCAB AC  B  A D D DB") == 0)
  }

  def getExits(pos: String) = Board(pos).exits
  def solve(start: String) = dijkstra(start, "AABBCCDD")(getExits)._1
  def solve4(start: String) = dijkstra(start, "AAAABBBBCCCCDDDD")(pos => Board(pos, 4).exits)._1

  def main(args: Array[String]): Unit = {
    test()

    val problem = readFile("day_23_input.txt")
    val result = solve(problem)
    checkAnswer(23, 1, result)

    def unfold(x: String) = {
      x.substring(0,1) + "DD" + x.substring(1,3) + "BC" + x.substring(3,5) + "AB" + x.substring(5,7) + "CA" + x.substring(7,8)
    }
    val result4 = solve4(unfold(problem))
    checkAnswer(23, 2, result4)
  }
}
