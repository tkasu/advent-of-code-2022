package tkasu.aoc22.challenges.day5

import cats.effect.IOApp
import tkasu.aoc22.challenges.day5.part1.{Stack, Move, parseFile, topCrates}

object part2 extends IOApp.Simple {

  object MoveExtensions {
    extension (move: Move) {
      def execCrateMover9001(stacks: Seq[Stack]): Seq[Stack] =
        val fromIdx   = move.from - 1
        val toIdx     = move.to - 1
        val fromStack = stacks(fromIdx)
        val toStack   = stacks(toIdx)

        stacks
          .updated(fromIdx, fromStack.copy(crates = fromStack.crates.drop(move.count)))
          .updated(
            toIdx,
            toStack.copy(crates = fromStack.crates.take(move.count) ::: toStack.crates)
          )
    }
  }

  import MoveExtensions._

  def execMoves(stacks: Seq[Stack], moves: Seq[Move]): Seq[Stack] =
    moves.foldLeft(stacks)((stacksState, move) => move.execCrateMover9001(stacksState))

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
