package tkasu.aoc22.challenges.day4

import cats.effect.{IO, IOApp, Resource}
import tkasu.aoc22.challenges.day4.part1.{parseFile, CleaningPair, Elf}
import tkasu.aoc22.utils.files.{closeFile, makeSourceResource, readLines, readResource}

import java.io.File
import scala.io.Source

object part2 extends IOApp.Simple {

  object ElfExtensions {
    extension (elf: Elf) {
      def overlapsWith(other: Elf): Boolean =
        (elf.sections & other.sections).nonEmpty
    }
  }

  import ElfExtensions._

  object CleaningPairExtensions {
    extension (pair: CleaningPair) {
      def elvesOverlap: Boolean =
        pair.fst.overlapsWith(pair.snd)
    }
  }

  import CleaningPairExtensions._

  override def run = {
    parseFile()
      .map(_.filter(_.elvesOverlap))
      .map(_.size)
      .map(println)
  }
}
