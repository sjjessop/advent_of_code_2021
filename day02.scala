object day02 {

  import day01.{readLines, checkAnswer}

  object Verbs extends Enumeration {
    val forward, up, down = Value
  }
  type Verb = Verbs.Value

  case class Command(verb: Verb, value: Int) {}

  object Command {
    def parse(cmd: String): Command = {
      cmd.split(" ").toList match {
        case (verb::value::Nil) => Command(Verbs.withName(verb), value.toInt)
        case _ => throw new Exception("bad")
      }
    }
  }

  case class Position(distance: Int = 0, depth: Int = 0) {
    def product = distance * depth
    def exec(cmd: Command): Position = cmd.verb match {
      case Verbs.forward => Position(distance + cmd.value, depth)
      case Verbs.up => Position(distance, depth - cmd.value)
      case Verbs.down => Position(distance, depth + cmd.value)
    }
  }

  case class AimedPosition(distance: Long = 0, depth: Long = 0, aim: Long = 0) {
    def product = distance * depth
    def exec(cmd: Command): AimedPosition = cmd.verb match {
      case Verbs.forward => AimedPosition(distance + cmd.value, depth + aim * cmd.value, aim)
      case Verbs.up => AimedPosition(distance, depth, aim - cmd.value)
      case Verbs.down => AimedPosition(distance, depth, aim + cmd.value)
    }
  }

  def main(args: Array[String]): Unit = {
    val commands = readLines("day_02_input.txt").map(Command.parse)
    val result = commands.foldLeft(Position())((pos, cmd) => pos.exec(cmd)).product
    checkAnswer(2, 1, result)

    val result2 = commands.foldLeft(AimedPosition())((pos, cmd) => pos.exec(cmd)).product
    checkAnswer(2, 2, result2)
  }
}
