object day17 {

  import day01.{readLines, checkAnswer}
  import day05.{Pos, Direction}

  def triangle(n: Int) = (n * (n + 1)) / 2

  case class Target(left: Int, right: Int, top: Int, bottom: Int) {
    assert(left <= right)
    assert(bottom <= top)
    assert(top < 0)
    def contains(pos: Pos): Boolean = (
      pos.x <= right && pos.x >= left && pos.y <= top && pos.y >= bottom
    )
    val minX = (0 to 1000).view.filter(dx => triangle(dx) >= left).head
    assert(triangle(minX) >= left)
    assert(triangle(minX - 1) < left)
  }

  object Target {
    def parse(value: String): Target = {
      val pattern = "target area: x=([-0-9]+)..([-0-9]+), y=([-0-9]+)..([-0-9]+)".r
      value match {
        case pattern(left, right, bottom, top) =>
          Target(left.toInt, right.toInt, top.toInt, bottom.toInt)
      }
    }
  }

  case class Probe(pos: Pos, velocity: Direction) {}

  case class Path(dx: Int, dy: Int) {
    assert(dy >= 0)

    // The furthest the probe can travel in the x-direction, due to drag
    val maxX = triangle(dx)
    // y-direction is unimpeded by drag(!), but if we start upwards and
    // accelerate down then it's still bounded above.
    val maxY = triangle(dy)

    // If the probe starts with velocity (dx,dy), dy >= 0, then at time
    // 2 * dy + 1 it is back on the x-axis. Call this step "reentry". Since the
    // target is below the x-axis, it has not hit the target yet.
    val reentry: Probe = after(2 * dy + 1)
    assert(reentry.pos.y == 0)

    def after(t: Int): Probe = {
      val velocity = Direction(math.max(dx - t, 0), dy - t)
      val position = Pos(
        if (velocity.x == 0) maxX else maxX - triangle(velocity.x),
        t * dy - triangle(t - 1),
      )
      Probe(position, velocity)
    }

    def hits(target: Target): Boolean = {
      val steps = Iterator.unfold(2 * dy + 1)(t => {
        val probe = after(t)
        if (probe.pos.y < target.bottom) None else Some((probe, t+1))
      })
      steps.map(probe => target.contains(probe.pos)).contains(true)
    }
  }

  def test(): Unit = {
    assert(Path(7, 2).reentry == Probe(Pos(25, 0), Direction(2, -3)))
  }

  def main(args: Array[String]): Unit = {
    test()

    val target = Target.parse(readLines("day_17_input.txt").head)
    // Bound the search space: this code isn't really necessary since we don't
    // actually need the optimisation, but I found it interesting.
    val paths = for (
      // dy is bounded below by 0 because any path with dy <= 0 has maxY 0,
      // which is boring. That's probably not the correct answer, but we can
      // have it as a back-up if we don't find anything else.
      // dy is bounded above by -target.bottom, because travelling at (-dy - 1)
      // at re-entry, the probe would clear the target in one step.
      dy <- (-target.bottom to 0 by -1).view;
      // dx is bounded below by the fact we must reach the target.
      // dx upper bound could be tighter based on dy (see the first filter),
      // but this is enough to overshoot in 2 steps, which is needed to reach
      // negative y, let alone the target.
      dx <- target.minX to (target.right + 1 / 2) + 1
    ) yield Path(dx, dy)
    val inBounds = paths.filter(_.reentry.pos.x <= target.right)
    val winners = paths.filter(_.hits(target))
    val result = winners.head.maxY
    checkAnswer(17, 1, result)
  }
}
