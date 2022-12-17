package tkasu.aoc22.challenges.day13

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

object part1 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day13/part1/input_example")
  private val inputResource = makeSourceResource("day13/part1/input")

  sealed trait PacketData
  case class PacketInt(value: Int)                extends PacketData
  case class PacketList(values: List[PacketData]) extends PacketData

  object PacketList:
    def fromRaw(line: String): PacketList =
      def helper(toParse: String, parseState: PacketList, parseStateLen: Int): (PacketList, Int) =
        if (toParse == "") (parseState.copy(values = parseState.values.reverse), parseStateLen)
        else if (toParse.startsWith("]"))
          val parseStateFinal = parseState.copy(values = parseState.values.reverse)
          (parseStateFinal, parseStateLen + 1)
        else if (toParse.startsWith(",")) helper(toParse.drop(1), parseState, parseStateLen + 1)
        else if (toParse.startsWith("["))
          val (newList, newListParseLen) = helper(toParse.drop(1), PacketList(List.empty), 1)
          val parseStateUpdated          = parseState.copy(values = newList :: parseState.values)
          helper(toParse.drop(newListParseLen), parseStateUpdated, parseStateLen + newListParseLen)
        else
          val (next, rest)      = toParse.span(c => !Set(',', ']').contains(c))
          val newPacket         = PacketInt(next.toInt)
          val parseStateUpdated = parseState.copy(values = newPacket :: parseState.values)
          helper(rest, parseStateUpdated, parseStateLen + next.length)

      helper(line.drop(1), PacketList(List.empty), 1)._1

  case class Pair(left: PacketList, right: PacketList) {

    def inRightOrder(): Option[Boolean] =
      val leftNext  = left.values.headOption
      val rightNext = right.values.headOption
      (leftNext, rightNext) match {
        case (None, None)    => None
        case (None, Some(_)) => Some(true)
        case (Some(_), None) => Some(false)
        case (Some(PacketInt(fstInt)), Some(PacketInt(sndInt))) =>
          if (fstInt < sndInt) Some(true)
          else if (fstInt > sndInt) Some(false)
          else Pair(PacketList(left.values.tail), PacketList(right.values.tail)).inRightOrder()
        case (Some(leftList @ PacketList(_)), Some(rightList @ PacketList(_))) =>
          Pair(leftList, rightList).inRightOrder() match {
            case Some(true)  => Some(true)
            case Some(false) => Some(false)
            case None =>
              Pair(PacketList(left.values.tail), PacketList(right.values.tail)).inRightOrder()
          }
        case (Some(leftList @ PacketList(_)), Some(rightInt @ PacketInt(_))) =>
          Pair(leftList, PacketList(List(rightInt))).inRightOrder() match {
            case Some(true)  => Some(true)
            case Some(false) => Some(false)
            case None =>
              Pair(PacketList(left.values.tail), PacketList(right.values.tail)).inRightOrder()
          }
        case (Some(leftInt @ PacketInt(_)), Some(rightList @ PacketList(_))) =>
          Pair(PacketList(List(leftInt)), rightList).inRightOrder()

      }
  }

  object Pair:
    def fromRaw(input: String) =
      val rawLeft :: rawRight :: _ = input.split("\n").toList
      val left                     = PacketList.fromRaw(rawLeft)
      val right                    = PacketList.fromRaw(rawRight)
      Pair(left, right)

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    pairs = input.split("\n\n").map(Pair.fromRaw)
  } yield pairs.toSeq

  override def run = for {
    parsed <- parseFile()
    // _      <- IO(println(parsed))
    inRightOrder = parsed.zipWithIndex.map((pair, idx) => (idx + 1, pair.inRightOrder()))
    // _ <- IO(println(inRightOrder))
    sumOfRightOrderIndices = inRightOrder
      .filter((_, isRightOrder) => isRightOrder.contains(true))
      .map((pairId, _) => pairId)
      .sum
    _ <- IO(println(sumOfRightOrderIndices))
  } yield ()
}
