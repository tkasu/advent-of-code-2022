package tkasu.aoc22.challenges.day4

import cats.effect.{IO, IOApp, Resource}
import tkasu.aoc22.utils.files.{closeFile, makeSourceResource, readLines, readResource}

import java.io.File
import scala.collection.SortedSet
import scala.io.Source

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day4/part1/input")

  case class Elf(sections: SortedSet[Int]) {
    def isUselessComparedTo(other: Elf): Boolean =
      sections.diff(other.sections).isEmpty
  }

  object Elf:
    def fromRaw(s: String): Elf =
      val (beg, end) = s.split("-") match {
        case Array(beg, end, _*) => (beg.toInt, end.toInt)
        case _ => throw IllegalAccessException(s"Failed to parse Elf from input: $s")
      }
      Elf(SortedSet.from(beg to end))

  case class CleaningPair(fst: Elf, snd: Elf) {
    def hasUselessElf: Boolean = {
      fst.isUselessComparedTo(snd) || snd.isUselessComparedTo(fst)
    }
  }

  object CleaningPair:
    def fromRaw(line: String): CleaningPair =
      val (fst, snd) = line.split(",") match {
        case Array(fst, snd, _*) => (fst, snd)
        case _ => throw IllegalAccessException(s"Failed to parse line from input: $line")
      }
      CleaningPair(Elf.fromRaw(fst), Elf.fromRaw(snd))

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    pairs = input.split("\n").map(CleaningPair.fromRaw)
  } yield pairs.toSeq

  override def run = {
    parseFile()
      .map(_.filter(_.hasUselessElf))
      .map(_.size)
      .map(println)
  }
}
