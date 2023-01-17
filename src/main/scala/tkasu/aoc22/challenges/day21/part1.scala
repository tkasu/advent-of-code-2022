package tkasu.aoc22.challenges.day21

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

import scala.util.Try

object part1 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day21/part1/input_example")
  private val inputResource = makeSourceResource("day21/part1/input")

  val ROOT_KEY = "root"

  enum MathOp:
    case Plus, Minus, Multiply, Divide

  object MathOp:

    def calc(op: MathOp, left: BigInt, right: BigInt): BigInt =
      op match {
        case MathOp.Plus     => left + right
        case MathOp.Minus    => left - right
        case MathOp.Multiply => left * right
        case MathOp.Divide   => left / right
      }

  case class Monkey(id: String, action: Yell)

  object Monkey:

    def fromRaw(line: String): Monkey =
      val id :: yellRaw :: Nil = line.split(": ").toList
      val yell = yellRaw.split(" ").toList match {
        case leftId :: "+" :: rightId :: Nil => Yell.Operation(MathOp.Plus, leftId, rightId)
        case leftId :: "-" :: rightId :: Nil => Yell.Operation(MathOp.Minus, leftId, rightId)
        case leftId :: "*" :: rightId :: Nil => Yell.Operation(MathOp.Multiply, leftId, rightId)
        case leftId :: "/" :: rightId :: Nil => Yell.Operation(MathOp.Divide, leftId, rightId)
        case number :: Nil if Try(number.toInt).isSuccess => Yell.Number(number.toInt)
        case _ => throw IllegalArgumentException(s"Could not parse line $line")
      }
      Monkey(id, yell)

  enum Yell:
    case Number(value: BigInt)
    case Operation(op: MathOp, leftMonkeyId: String, rightMonkeyId: String)

  type MonkeyMap = Map[String, Monkey]

  def solve(key: String, monkeyMap: MonkeyMap): BigInt =
    val monkey = monkeyMap(key)
    monkey.action match {
      case Yell.Number(number) => number
      case Yell.Operation(op, leftId, rightId) =>
        MathOp.calc(
          op,
          solve(leftId, monkeyMap),
          solve(rightId, monkeyMap)
        )
    }

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    monkeys   = input.split("\n").map(Monkey.fromRaw)
    monkeyMap = monkeys.map(monkey => monkey.id -> monkey).toMap
  } yield monkeyMap

  override def run = for {
    monkeyMap <- parseFile()
    _         <- IO(println(monkeyMap))
    solution = solve(ROOT_KEY, monkeyMap)
    _ <- IO(println(solution))
  } yield ()
}
