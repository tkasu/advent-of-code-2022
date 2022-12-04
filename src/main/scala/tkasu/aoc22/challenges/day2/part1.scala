package tkasu.aoc22.challenges.day2

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day2/part1/input")

  enum RPCInput:
    case Rock, Paper, Scissors

  object RPCInput:
    def fromOppRaw(s: String): RPCInput =
      s match {
        case "A" => RPCInput.Rock
        case "B" => RPCInput.Paper
        case "C" => RPCInput.Scissors
        case _   => throw IllegalArgumentException(s"Invalid opponent input: $s")
      }

    def fromMyRaw(s: String): RPCInput =
      s match {
        case "X" => RPCInput.Rock
        case "Y" => RPCInput.Paper
        case "Z" => RPCInput.Scissors
        case _   => throw IllegalArgumentException(s"Invalid my input: $s")
      }

  case class RPCRound(otherInput: RPCInput, myInput: RPCInput) {

    def shapeScore: Int = this.myInput match {
      case RPCInput.Rock     => 1
      case RPCInput.Paper    => 2
      case RPCInput.Scissors => 3
    }

    def perfScore = this match {
      case RPCRound(other, my) if other == my => 3
      case RPCRound(RPCInput.Rock, my) =>
        my match {
          case RPCInput.Paper    => 6
          case RPCInput.Scissors => 0
        }
      case RPCRound(RPCInput.Paper, my) =>
        my match {
          case RPCInput.Scissors => 6
          case RPCInput.Rock     => 0
        }
      case RPCRound(RPCInput.Scissors, my) =>
        my match {
          case RPCInput.Rock  => 6
          case RPCInput.Paper => 0
        }
    }

    def score: Int =
      shapeScore + perfScore
  }

  object RPCRound:

    /** Parse input from AoC input format: e.g. 'A X' First other players is my input: A -> Rock B
      * -> Paper C -> Scissors After space, the second latter is my input: X -> Rock Y -> Paper Z ->
      * Scissors
      */
    def fromRaw(lineInput: String): RPCRound =
      val (oppRaw, myRaw) = lineInput.split(" ") match {
        case Array(fst, snd, _*) => (fst, snd)
        case _                   => throw IllegalArgumentException(s"Invalid input row: $lineInput")
      }

      val oppInput = RPCInput.fromOppRaw(oppRaw)
      val myInput  = RPCInput.fromMyRaw(myRaw)
      RPCRound(oppInput, myInput)

  def parseFile(): IO[Seq[RPCRound]] = for {
    input <- inputResource.use(src => readLines(src))
    rounds = input
      .split("\n")
      .map(RPCRound.fromRaw)
      .toSeq
  } yield rounds

  override def run = {
    parseFile()
      .map(_.map(_.score).sum)
      .map(println)
  }
}
