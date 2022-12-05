package tkasu.aoc22.challenges.day5

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

import scala.util.matching.Regex

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day5/part1/input")

  case class Move(count: Int, from: Int, to: Int) {

    private def singleMove(fromCrates: List[Char], toCrates: List[Char]): (List[Char], List[Char]) =
      (fromCrates.drop(1), fromCrates.head +: toCrates)

    def exec(stacks: Seq[Stack]): Seq[Stack] =
      val fromIdx   = from - 1
      val toIdx     = to - 1
      val fromStack = stacks(fromIdx)
      val toStack   = stacks(toIdx)

      val (fromNewCrates, toNewCrates) =
        (0 until count).foldLeft(fromStack.crates, toStack.crates)((crateTuple, _) =>
          singleMove(crateTuple._1, crateTuple._2)
        )

      stacks
        .updated(fromIdx, fromStack.copy(crates = fromNewCrates))
        .updated(toIdx, toStack.copy(crates = toNewCrates))
  }

  object Move:
    val movePattern: Regex = """^move\s(\d+)\sfrom\s(\d+)\sto\s(\d+)$""".r

    def fromRaw(line: String): Move = line match {
      case movePattern(move, from, to) => Move(move.toInt, from.toInt, to.toInt)
    }

  case class Stack(id: Int, crates: List[Char])

  object Stack:
    val STACK_INPUT_WIDTH          = 3
    val SPACE_BETWEEN_ID_INPUTS    = 3
    val SPACE_BETWEEN_STACK_INPUTS = 1

    private def parseIdRow(s: String): List[Int] =
      s.trim
        .split(" " * SPACE_BETWEEN_ID_INPUTS)
        .map(_.toInt)
        .toList

    private def parseCrateRows(rows: List[String], ids: List[Int]): Seq[Stack] =
      for (id, idx) <- ids.zipWithIndex yield
        val rowStartIdx = idx * (SPACE_BETWEEN_STACK_INPUTS + STACK_INPUT_WIDTH)
        val rowEndIdx   = rowStartIdx + STACK_INPUT_WIDTH
        val crates = rows
          .map(_.slice(rowStartIdx, rowEndIdx))
          .map(_.trim)
          .filterNot(_.isBlank)
          .map(_.replace("[", ""))
          .map(_.replace("]", ""))
          .map(_.head)
        Stack(id, crates)

    def stacksFromRaw(s: String) =
      val rows   = s.split("\n").toList
      val ids    = parseIdRow(rows.last)
      val stacks = parseCrateRows(rows.init, ids)
      stacks

  def parseFile(): IO[(Seq[Stack], Seq[Move])] = for {
    input <- inputResource.use(src => readLines(src))
    stacksRaw :: movesRaw :: _ = input.split("\n\n").toList
    stacks                     = Stack.stacksFromRaw(stacksRaw)
    moves                      = movesRaw.split("\n").map(Move.fromRaw).toSeq
  } yield (stacks, moves)

  def execMoves(stacks: Seq[Stack], moves: Seq[Move]): Seq[Stack] =
    moves.foldLeft(stacks)((stacksState, move) => move.exec(stacksState))

  def topCrates(stacks: List[Stack]): List[Char] =
    stacks.map { stack =>
      stack.crates match
        case List()   => ' '
        case fst :: _ => fst
    }

  override def run = {
    parseFile()
      .map { (stacks, moves) =>
        execMoves(stacks, moves).toList
      }
      .map(topCrates)
      .map(_.mkString(""))
      .map(println)
  }
}
