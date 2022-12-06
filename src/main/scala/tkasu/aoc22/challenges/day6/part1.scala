package tkasu.aoc22.challenges.day6

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day6/part1/input")

  case class Signal(buffer: Array[Char]) {
    private val START_OF_PACKET_CHARS = 4

    def startOfPacketIdx(): Option[Int] =
      startOfMarker(START_OF_PACKET_CHARS)

    def startOfMarker(uniqueCharsNeeded: Int): Option[Int] =
      val candidates           = startOfCandidates(uniqueCharsNeeded)
      val candidateUniqueChars = candidates.map(_.toSet.size)
      candidateUniqueChars.zipWithIndex
        .find { (len, _) =>
          len == uniqueCharsNeeded
        }
        // reported value is when start of markers have been finished
        .map(_._2 + uniqueCharsNeeded)

    private def startOfCandidates(uniqueCharsNeeded: Int): Seq[Array[Char]] =
      for idx <- (0 to buffer.length - uniqueCharsNeeded)
      yield buffer.slice(idx, idx + uniqueCharsNeeded)

    override def toString = buffer.mkString("Signal(", ",", ")")
  }

  def parseFile(): IO[Signal] = for {
    input <- inputResource.use(src => readLines(src))
    signal = Signal(input.toCharArray)
  } yield signal

  override def run = {
    parseFile()
      .map(_.startOfPacketIdx())
      .map(println)
  }
}
