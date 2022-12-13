package tkasu.aoc22.challenges.day10

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.matrix._
import tkasu.aoc22.challenges.day10.part1.{Program, parseFile}

import scala.annotation.tailrec

object part2 extends IOApp.Simple {

  case class CRT(regValue: Int, time: Int, crtRow: Seq[Char]) {
    val WIDTH: Int = 40

    val stripePos: (Int, Int, Int) = getStripePos(regValue)

    def crtMatrix: Array[Array[Char]] =
      @tailrec
      def helper(crtRowRem: Seq[Char], matrixBuilder: Array[Array[Char]]): Array[Array[Char]] =
        if (crtRowRem.size <= WIDTH) matrixBuilder :+ crtRowRem.toArray
        else
          val (newRow, rest) = crtRowRem.splitAt(WIDTH)
          helper(rest, matrixBuilder :+ newRow.toArray)

      helper(crtRow, Array.empty)

    private def getStripePos(value: Int): (Int, Int, Int) =
      (
        Math.max(normalizeScreenValue(value - 1), 0),
        normalizeScreenValue(value),
        Math.min(normalizeScreenValue(value + 1), WIDTH - 1)
      )

    private def normalizeScreenValue(value: Int): Int = value % WIDTH

    def execCycles(cycles: Int, newRegValue: Int): CRT =
      def getScreenChar(t: Int): Char =
        if (stripePos.productIterator.contains(normalizeScreenValue(t - 1))) '#' else ' '

      val charsUntilNextState = (time until (time + cycles)).map(getScreenChar)
      CRT(newRegValue, time + cycles, crtRow ++ charsUntilNextState)

    override def toString: String = stringifyMatrixNoSep(crtMatrix, "CRT")
  }

  object CRT:
    def fromProgram(program: Program): CRT =
      val initialCrtState: CRT = CRT(0, 1, Seq.empty)
      program.states.foldLeft(initialCrtState) { case (crtState, nextProgState) =>
        crtState.execCycles(nextProgState.time - crtState.time, nextProgState.regValue)
      }

  override def run = for {
    instructions <- parseFile()
    program = Program(instructions)
    crt     = CRT.fromProgram(program)
    _ <- IO(println(crt))
  } yield ()
}
