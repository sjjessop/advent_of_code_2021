object day21 {

  import day01.{readLines, checkAnswer}

  import collection.mutable

  sealed case class Game(playerPosition: Seq[Int], playerScore: Seq[Int] = Seq(0, 0), movePlayer: Int = 0, rolls: Int = 0, target: Int = 1000) {
    def move(roll: List[Int]): Game = {
      if (finished) this
      else {
        // println(s"roll ${roll}")
        val newPos = (((playerPosition(movePlayer) + roll.sum) - 1) % 10) + 1
        val newScore = playerScore(movePlayer) + newPos
        Game(
          playerPosition.updated(movePlayer, newPos),
          playerScore.updated(movePlayer, newScore),
          (movePlayer + 1) % playerPosition.size,
          rolls + roll.size,
          target,
        )
      }
    }
    def finished: Boolean = playerScore.max >= target
    def play(die: Iterator[List[Int]]): Game = {
      // Warning - impure - consumes the argument "die"
      die.scanLeft(this)((game, roll) => game.move(roll)).dropWhile(!_.finished).next()
    }
    def result: Int = playerScore.min * rolls
  }

  def d100() = Iterator.continually(1 to 100).flatten.grouped(3).map(_.toList)

  def test(): Unit = {
    val testGame = Game(Seq(4,8))
    assert(testGame.play(d100()).result == 745 * 993)
  }

  def main(args: Array[String]): Unit = {
    test()

    val startPos = readLines("day_21_input.txt").map(_.split(": ")(1).toInt)
    val realGame = Game(startPos)
    val result = realGame.play(d100()).result
    checkAnswer(21, 1, result)

    val rollThrice = (
      for (a <- 1 to 3; b <- 1 to 3; c <- 1 to 3) yield List(a,b,c)
    ).groupBy(_.sum).view.mapValues(_.size).toMap

    @annotation.tailrec
    def loop(universes: Map[Game, Long], winners: Seq[Long] = Seq(0, 0)): Seq[Long] = {
      // println(s"${universes.size} ${winners}")
      if (universes.isEmpty) winners
      else {
        val games = for ((game, count) <- universes.view; (roll, rollCount) <- rollThrice)
          yield (game.move(List(roll)), count * rollCount)
        val next = {
          // Accumulate still-running games and (separately) winners
          val mutableUniverses = mutable.Map.empty[Game, Long]
          val mutableWinners = winners.to(mutable.Seq)
          for ((game, count) <- games) {
            if (game.finished) {
              val winner = game.movePlayer
              mutableWinners(winner) = mutableWinners(winner) + count
            } else {
              mutableUniverses(game) = mutableUniverses.getOrElse(game, 0L) + count
            }
          }
          (mutableUniverses.toMap, mutableWinners.toSeq)
        }
        loop(next._1, next._2)
      }
    }

    val result2 = loop(Map(realGame.copy(target=21) -> 1L)).max
    checkAnswer(21, 2, result2)
  }
}
