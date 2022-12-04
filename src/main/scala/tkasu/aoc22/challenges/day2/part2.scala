package tkasu.aoc22.challenges.day2

import cats.effect.{IO, IOApp}
import tkasu.aoc22.challenges.day2.part1.{RPCInput, RPCRound}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

object part2 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day2/part1/input")

  enum RoundResult:
    case Lose, Draw, Win

  object RoundResult:
    def fromRaw(s: String): RoundResult =
      s match {
        case "X" => RoundResult.Lose
        case "Y" => RoundResult.Draw
        case "Z" => RoundResult.Win
        case _   => throw IllegalArgumentException(s"Invalid result input: $s")
      }

  def fromResultRaw(s: String, oppInput: RPCInput): RPCInput =
    RoundResult.fromRaw(s) match {
      case RoundResult.Draw => oppInput
      case RoundResult.Win =>
        oppInput match {
          case RPCInput.Rock     => RPCInput.Paper
          case RPCInput.Paper    => RPCInput.Scissors
          case RPCInput.Scissors => RPCInput.Rock
        }
      case RoundResult.Lose =>
        oppInput match {
          case RPCInput.Rock     => RPCInput.Scissors
          case RPCInput.Paper    => RPCInput.Rock
          case RPCInput.Scissors => RPCInput.Paper
        }
    }

  /** Parse input from AoC input format: e.g. 'A X' First other players is my input: A -> Rock B ->
    * Paper C -> Scissors After space, the desired result for the round: X -> Lose Y -> Draw Z ->
    * Win
    */
  def roundFromRaw(lineInput: String): RPCRound =
    val (oppRaw, resultRaw) = lineInput.split(" ") match {
      case Array(fst, snd, _*) => (fst, snd)
      case _                   => throw IllegalArgumentException(s"Invalid input row: $lineInput")
    }

    val oppInput = RPCInput.fromOppRaw(oppRaw)
    val myInput  = fromResultRaw(resultRaw, oppInput)
    RPCRound(oppInput, myInput)

  def parseFile(): IO[Seq[RPCRound]] = for {
    input <- inputResource.use(src => readLines(src))
    rounds = input
      .split("\n")
      .map(roundFromRaw)
      .toSeq
  } yield rounds

  override def run = {
    parseFile()
      .map(_.map(_.score).sum)
      .map(println)
  }
}
