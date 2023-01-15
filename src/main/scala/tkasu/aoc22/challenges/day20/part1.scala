package tkasu.aoc22.challenges.day20

import scala.annotation.tailrec
import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

object part1 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day20/part1/input_example")
  private val inputResource = makeSourceResource("day20/part1/input")

  case class EncryptedFile(numbers: Vector[Number], nextShuffleId: Int = 0):
    lazy val baseIdx = findId(numbers.find(_.value == 0).get.id)._2

    def valueAtCoordinate(id: Int): Number =
      val idx = (baseIdx + id) % numbers.size
      numbers(idx)

    def findId(id: Int): (Number, Int) =
      numbers.zipWithIndex.find { case (num, _) =>
        num.id == id
      }.get

    @tailrec
    private def adjustIdx(idx: Int): Int =
      val newIdx =
        if (idx == 0) numbers.size - 1
        else if (idx < 0) (numbers.size - 1) + idx
        else if (idx == numbers.size) 0
        else if (idx >= numbers.size) (idx - numbers.size) + 1
        else idx
      if (newIdx >= 0 && (newIdx < numbers.size)) newIdx
      else adjustIdx(newIdx)

    def shuffleNext(): EncryptedFile =
      val (number, curIdx)   = findId(nextShuffleId)
      val newIdxWithOverflow = curIdx + number.value
      val newIdx             = adjustIdx(newIdxWithOverflow)

      if (newIdx < 0 || (newIdx >= numbers.size))
        throw RuntimeException(
          s"Invalid index when processing id $nextShuffleId, got index $newIdx}"
        )

      val newNumbers =
        if (newIdx < curIdx)
          numbers.slice(0, newIdx)
            ++: Vector(number)
            ++: numbers.slice(newIdx, curIdx)
            ++: numbers.slice(curIdx + 1, numbers.size)
        else
          numbers.slice(0, curIdx)
            ++: numbers.slice(curIdx + 1, newIdx + 1)
            ++: Vector(number)
            ++: numbers.slice(newIdx + 1, numbers.size)
      if (newNumbers.size != numbers.size)
        throw RuntimeException(
          s"Size of the vector was altered when processing id $nextShuffleId, old size ${numbers.size}, new size: ${newNumbers.size}"
        )
      EncryptedFile(newNumbers, nextShuffleId + 1)

    @tailrec
    final def shuffleAll(): EncryptedFile =
      if (nextShuffleId >= numbers.size) this
      else
        if (nextShuffleId % 100 == 0) println(s"Shuffling id $nextShuffleId")
        this.shuffleNext().shuffleAll()

  case class Number(id: Int, value: Int)

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    numbers = input
      .split("\n")
      .zipWithIndex
      .map { case (numStr, idx) =>
        Number(idx, numStr.toInt)
      }
      .toVector
  } yield EncryptedFile(numbers)

  override def run = for {
    encrypted <- parseFile()
    decrypted = encrypted.shuffleAll()
    _ <- IO(println(decrypted))
    valuesAtCoords = Seq(1000, 2000, 3000).map(decrypted.valueAtCoordinate).map(_.value)
    _ <- IO(println(s"Values at coords: $valuesAtCoords, sum: ${valuesAtCoords.sum}"))
  } yield ()
}
