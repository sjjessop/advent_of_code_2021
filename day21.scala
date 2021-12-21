object day21 {

  import day01.{readLines, checkAnswer}

  sealed case class Game(playerPosition: Seq[Int], playerScore: Seq[Int] = Seq(0, 0), movePlayer: Int = 0, rolls: Int = 0) {
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
        )
      }
    }
    def finished: Boolean = playerScore.max >= 1000
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
  }
}
