package tkasu.aoc22.challenges.day6

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}
import tkasu.aoc22.challenges.day6.part1.{parseFile, Signal}

object part2 extends IOApp.Simple {

  object SignalExtensions {
    extension (signal: Signal) {
      def START_OF_MESSAGE_CHARS = 14

      def startOfMessageIdx(): Option[Int] =
        signal.startOfMarker(START_OF_MESSAGE_CHARS)
    }
  }

  import SignalExtensions._

  override def run = {
    parseFile()
      .map(_.startOfMessageIdx())
      .map(println)
  }
}
