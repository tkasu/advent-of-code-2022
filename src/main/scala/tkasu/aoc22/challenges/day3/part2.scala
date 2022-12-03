package tkasu.aoc22.challenges.day3

import cats.effect.{IO, IOApp, Resource}
import tkasu.aoc22.challenges.day3.part1.{parseFile, priorityMap, RuckSack}
import tkasu.aoc22.utils.files.{closeFile, makeSourceResource, readLines, readResource}

import java.io.File
import scala.io.Source

object part2 extends IOApp.Simple {

  def sumOfBadgePriorities(badges: Seq[Char]): Int =
    badges
      .map(priorityMap)
      .sum

  def toGroups(sacks: Seq[RuckSack]): Seq[Seq[RuckSack]] =
    sacks.grouped(3).toSeq

  def findBadge(sackGroup: Seq[RuckSack]): Char =
    val sackGroupChars = sackGroup.map(sack => Set.from(sack.allItems))
    val common = sackGroupChars.reduce((acc, next) => acc & next)
    if (common.size != 1) {
      throw IllegalArgumentException(s"Expected exactly one common characters: ${common.mkString(",")} from $sackGroup")
    }
    common.head

  override def run = {
    parseFile()
      .map(toGroups)
      .map(_.map(findBadge))
      .map(sumOfBadgePriorities)
      .map(println)
  }
}
