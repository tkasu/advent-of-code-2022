package tkasu.aoc22.challenges.day10

import scala.util.Try
import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

import scala.annotation.tailrec

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day10/part1/input")

  trait Instruction:
    val instructionLen: Int

  object Instruction:
    def fromLine(line: String): Instruction =
      val parts = line.split(" ").toList
      parts match {
        case "noop" :: Nil                                          => Noop()
        case "addx" :: amount :: Nil if Try(amount.toInt).isSuccess => Addx(amount.toInt)
        case _ => throw IllegalArgumentException(s"Unknown instruction line $parts.")
      }

  case class Noop() extends Instruction {
    override val instructionLen = 1
  }
  case class Addx(regInc: Int) extends Instruction {
    override val instructionLen = 2
  }

  case class ProgramState(instructionsQueue: Seq[Instruction], regValue: Int, time: Int) {

    def execInstruction(): ProgramState =
      instructionsQueue.head match {
        case noop: Noop =>
          ProgramState(instructionsQueue.tail, regValue, time + noop.instructionLen)
        case addx: Addx =>
          ProgramState(instructionsQueue.tail, regValue + addx.regInc, time + addx.instructionLen)
      }
  }

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    instructions = input.split("\n").map(Instruction.fromLine).toSeq
    program      = ProgramState(instructions, regValue = 1, time = 1)
  } yield program

  def runProgram(program: ProgramState): List[ProgramState] =
    @tailrec
    def helper(programState: ProgramState, executedStates: List[ProgramState]): List[ProgramState] =
      programState.instructionsQueue.toList match {
        case List() => executedStates
        case _ =>
          val newState = programState.execInstruction()
          helper(newState, newState :: executedStates)
      }
    helper(program, List(program)).reverse

  def regValueAtCycle(programStates: List[ProgramState], cycle: Int): Int =
    programStates
      .map(state => (state.time, state.regValue))
      .findLast((stateCycle, _) => (stateCycle <= cycle))
      .map((_, regValue) => regValue)
      .get

  override def run = for {
    initialProgram <- parseFile()
    programStates = runProgram(initialProgram)
    // _ <- IO(println(programStates.map(state => (state.time, state.regValue))))
    sigStrengths = List(20, 60, 100, 140, 180, 220).map(cycle =>
      cycle * regValueAtCycle(programStates, cycle)
    )
    // _ <- IO(println(s"Signal strengths: $sigStrengths"))
    _ <- IO(println(s"Sum of signal strengths: ${sigStrengths.sum}"))
  } yield ()
}
