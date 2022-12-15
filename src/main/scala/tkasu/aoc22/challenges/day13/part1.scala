package tkasu.aoc22.challenges.day13

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day13/part1/input_example")
  //private val inputResource = makeSourceResource("day13/part1/input")

  trait PacketData

  object PacketData:

    def fromRaw(line: String): PacketData =

      def helper(toParse: String, parseState: PacketList, parseStateLen: Int): (PacketList, Int) =
        if (toParse == "") (parseState.copy(values = parseState.values.reverse), parseStateLen)
        else if (toParse.startsWith("]"))
          val parseStateFinal = parseState.copy(values = parseState.values.reverse)
          (parseStateFinal, parseStateLen + 1)
        else if (toParse.startsWith(","))
          helper(toParse.drop(1), parseState, parseStateLen + 1)
        else if (toParse.startsWith("["))
          val (newList, newListParseLen) = helper(toParse.drop(1), PacketList(List.empty), 1)
          val parseStateUpdated = parseState.copy(values = newList :: parseState.values)
          helper(toParse.drop(newListParseLen), parseStateUpdated, parseStateLen + newListParseLen)
        else
          val (next, rest) = toParse.span(c => !Set(',', ']').contains(c))
          val newPacket = PacketInt(next.toInt)
          val parseStateUpdated = parseState.copy(values = newPacket :: parseState.values)
          helper(rest, parseStateUpdated, parseStateLen + next.length)

      helper(line.drop(1), PacketList(List.empty), 1)._1

  case class PacketInt(value: Int) extends PacketData
  case class PacketList(values: List[PacketData]) extends PacketData
  case class Pair(fst: PacketData, snd: PacketData)

  object Pair:
    def fromRaw(input: String) =
      val rawFst :: rawSnd :: _ = input.split("\n").toList
      val pairFst = PacketData.fromRaw(rawFst)
      val pairSnd = PacketData.fromRaw(rawSnd)
      Pair(pairFst, pairSnd)

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    pairs = input.split("\n\n").map(Pair.fromRaw)
  } yield pairs.toSeq

  override def run = for {
    parsed <- parseFile()
    _      <- IO(println(parsed))
  } yield ()
}
