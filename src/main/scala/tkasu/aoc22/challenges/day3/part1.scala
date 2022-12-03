package tkasu.aoc22.challenges.day3

import cats.effect.{IO, IOApp, Resource}
import tkasu.aoc22.utils.files.{closeFile, makeSourceResource, readLines, readResource}

import java.io.File
import scala.io.Source

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day3/part1/input")

  val priorityMap: Map[Char, Int] =
    val lowerPrios = ('a' to 'z').zip(1 to 26)
    val higherPrios = ('A' to 'Z').zip(27 to 52)
    (lowerPrios ++ higherPrios).toMap

  case class RuckSack(comp1: Seq[Char], comp2: Seq[Char]) {

    def allItems: Seq[Char] = comp1 ++ comp2

    def inBoth(): Set[Char] =
      Set.from(comp1) & Set.from(comp2)

  }

  object RuckSack:
    /**
     * Covert string to RuckSack.
     * E.g. vJrwpWtwJgWrhcsFMMfFFhFp ->
     * RuckSack(Seq(v,J,r,w,p,W,t,w,J,g,W,r), Seq(h,c,s,F,M,M,f,F,F,h,F,p))
     */
    def fromRaw(s: String): RuckSack =
      val arr = s.toCharArray
      val comp1Chars = arr.slice(0, arr.size / 2)
      val comp2Chars = arr.slice(arr.size / 2, arr.size)
      RuckSack(Seq.from(comp1Chars), Seq.from(comp2Chars))

  def parseFile(): IO[Seq[RuckSack]] = for {
    input <- inputResource.use(src => readLines(src))
    sacks = input.split("\n").map(RuckSack.fromRaw)
  } yield sacks.toSeq

  def sumOfRearrangmentPriorities(sacks: Seq[RuckSack]): Int =
    sacks
      .map(
        _.inBoth().map(char => priorityMap(char)).sum
      )
      .sum

  override def run = {
    parseFile()
      .map(sumOfRearrangmentPriorities)
      .map(println)
  }
}
