package tkasu.aoc22.challenges.day1

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

import java.io.File
import scala.io.Source

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day1/part1/input")

  case class Elf(id: Int, calories: Int)

  def parseFile(): IO[Seq[Elf]] = for {
    input <- inputResource.use(src => readLines(src))
    elfBatches = input.split("\n\n")
    calories = elfBatches.map { batch =>
      batch
        .split("\n")
        .map(_.toInt)
    }
    elfs = calories.zipWithIndex.map { case (calories, index) =>
      Elf(index + 1, calories.sum)
    }
  } yield elfs.toSeq

  def mostCalories(elfs: Seq[Elf]): IO[Elf] =
    IO.pure(elfs.sortBy(_.calories)(Ordering[Int].reverse).head)

  override def run = {
    parseFile()
      .flatMap { elfs =>
        mostCalories(elfs)
      }
      .map(println)
  }
}
