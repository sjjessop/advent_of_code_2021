object day16 {

  import day01.{readLines, checkAnswer}

  def unHex(hex: Char): List[Int] = {
    val asInt: Int = hex match {
      case ch if (ch >= '0' && ch <= '9') => (ch - '0')
      case ch if (ch >= 'A' && ch <= 'F') => (ch - 'A') + 10
    }
    ((3 to 0 by -1).map(idx => if (((1 << idx) & asInt) != 0) 1 else 0)).toList
  }

  def getBits(lines: List[String]): List[Int] = lines.flatMap(_.toList).flatMap(unHex)

  // Here's a monad I prepared earlier
  case class State[S,+A](run: S => (A,S)) {
    def apply(s: S) = run(s)
    def map[B](f: A => B): State[S, B] =
      State(s => {
        val (a, s2) = run(s)
        (f(a), s2)
      })
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, s2) = run(s)
        f(a)(s2)
      })
    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- this
        b <- sb
      } yield f(a, b)
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
      fs.foldRight(State.unit[S, List[A]](Nil))((next, sofar) => next.map2(sofar)(_::_))
    }
    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  // Set S = List[Int] to mean State objects that operate on our input list of bits.
  type Read[A] = State[List[Int], A]

  object Read {
    def apply[A](run: List[Int] => (A, List[Int])) = new Read(run)

    val bit: Read[Int] = Read(is => (is.head, is.tail))

    def rawBits(n: Int): Read[List[Int]] = Read(is => is.splitAt(n))

    def bits(n: Int): Read[Int] = {
      assert(n <= 30)
      State.sequence(List.fill(n)(bit))
        .map(bits => bits.foldLeft(0)((acc, b) => acc * 2 + b))
    }

    val literalParts: Read[List[Int]] = {
      bits(5).flatMap(x => {
        if (x <= 15)
          State.unit(List(x))
        else
          literalParts.map(tail => (x & 15)::tail)
      })
    }

    val literalValue: Read[Long] = {
      literalParts.map(is => {
        if (is.size > 15) throw new Exception("overflow")
        is.foldLeft(0L)((acc, n) => acc * 16 + n)
      })
    }

    def literalPacket(version: Int): Read[LiteralPacket] = for {
      value <- literalValue
    } yield LiteralPacket(version, value)

    def operatorByNum(version: Int, typeID: Int): Read[OperatorPacket] = for {
      packetCount <- bits(11)
      subPackets <- packets(packetCount)
    } yield OperatorPacket(version, typeID, subPackets)

    def operatorByBits(version: Int, typeID: Int): Read[OperatorPacket] = for {
      bitCount <- bits(15)
      subBits <- rawBits(bitCount)
    } yield OperatorPacket(version, typeID, allPackets(subBits))

    def operatorPacket(version: Int, typeID: Int): Read[OperatorPacket] = for {
      lengthTypeId <- bit
      packet <- if (lengthTypeId == 1) operatorByNum(version, typeID) else operatorByBits(version, typeID)
    } yield packet

    val packet: Read[Packet] = for {
      version <- bits(3)
      typeID <- bits(3)
      packet <- if (typeID == Packet.LITERAL) literalPacket(version) else operatorPacket(version, typeID)
    } yield packet

    def packets(n: Int): Read[List[Packet]] = State.sequence(List.fill(n)(packet))

    def allPackets(input: List[Int]): List[Packet] = {
      List.unfold(input){
        case Nil => None
        case is => Some(packet(is))
      }
    }
  }

  sealed trait Packet {
    def versionSum: Int
    def evaluate: Long
  }
  object Packet {
    val LITERAL = 4
  }
  case class LiteralPacket(version: Int, value: Long) extends Packet {
    val typeID = Packet.LITERAL
    def versionSum: Int = version
    def evaluate: Long = value
  }
  case class OperatorPacket(version: Int, typeID: Int, subPackets: List[Packet]) extends Packet {
    def versionSum: Int = version + subPackets.map(_.versionSum).sum

    lazy val subEvaluate = subPackets.map(_.evaluate)

    def subOp(func: (Long, Long) => Boolean) = subEvaluate match {
      case left::right::Nil => if (func(left, right)) 1 else 0
      case _ => throw new Exception("bad")
    }

    def evaluate: Long = typeID match {
      case 0 => subEvaluate.sum
      case 1 => subEvaluate.product
      case 2 => subEvaluate.min
      case 3 => subEvaluate.max
      case 5 => subOp(_ > _)
      case 6 => subOp(_ < _)
      case 7 => subOp(_ == _)
     }
  }

  def parse(s: String) = Read.packet(getBits(List(s)))._1

  def test(): Unit = {
    val bits = List(1, 0, 1, 0, 1, 1)
    assert(Read.bit(bits)._1 == bits.head)
    assert(Read.bit(bits)._2 == bits.tail)
    assert(Read.bits(1)(bits)._1 == bits.head)
    assert(Read.bits(2)(bits)._1 == 2 * bits(0) + bits(1))
    assert(Read.bits(3)(bits)._1 == 4 * bits(0) + 2 * bits(1) + bits(2))
    assert(Read.bits(4)(bits)._1 == 8 * bits(0) + 4 * bits(1) + 2 * bits(2) + bits(3))
    assert(Read.bits(4)(bits)._2 == bits.drop(4))

    assert(parse("D2FE28") == LiteralPacket(version=6, value=2021))
    assert(parse("38006F45291200") == OperatorPacket(1, 6, List(LiteralPacket(6,10), LiteralPacket(2, 20))))

    // How embarrassing
    val bigLiteralBits = "0001001111111111111111111111111111111111101111".toList.map(_.toString.toInt)
    assert(Read.literalValue(bigLiteralBits.drop(6))._1 == 4294967295L)
    assert(Read.literalPacket(0)(bigLiteralBits.drop(6))._1.value == 4294967295L)
    assert((Read.packet(bigLiteralBits)._1 match {case x: LiteralPacket => x.value; case _ => 0}) == 4294967295L)

    assert(parse("C200B40A82").evaluate == 3)
    assert(parse("04005AC33890").evaluate == 54)
    assert(parse("880086C3E88112").evaluate == 7)
    assert(parse("CE00C43D881120").evaluate == 9)
    assert(parse("D8005AC2A8F0").evaluate == 1)
    assert(parse("F600BC2D8F").evaluate == 0)
    assert(parse("9C005AC2F8F0").evaluate == 0)
    assert(parse("9C0141080250320F1802104A08").evaluate == 1)
  }

  def main(args: Array[String]): Unit = {
    test()

    val bits = getBits(readLines("day_16_input.txt"))
    val message = Read.packet(bits)._1
    val versionTotal = message.versionSum
    checkAnswer(16, 1, versionTotal)

    val versionEval = message.evaluate
    checkAnswer(16, 2, versionEval)
  }
}
