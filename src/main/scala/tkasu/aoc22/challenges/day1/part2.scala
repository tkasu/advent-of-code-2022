package tkasu.aoc22.challenges.day1

import cats.effect.{IO, IOApp}
import tkasu.aoc22.challenges.day1.part1.{Elf, parseFile}

object part2 extends IOApp.Simple {

  def mostNCalories(elfs: Seq[Elf], n: Int): IO[Seq[Elf]] =
    IO(elfs.sortBy(_.calories)(Ordering[Int].reverse).take(n))

  override def run = {
    parseFile()
      .flatMap { elfs =>
        mostNCalories(elfs, 3)
      }
      .map(_.map(_.calories).sum)
      .map {
        println
      }
  }
}
