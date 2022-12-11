package tkasu.aoc22.challenges.day11

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

import scala.util.Try
import scala.util.matching.Regex

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day11/part1/input_example")

  enum Operation:
    case Plus, Mult

  case class MoneyOperation(op: Operation, amount: Option[Int])

  case class DivisibleByOperator(denum: Int)
  case class TargetMonkey(id: Int)
  case class TestOperator(
      op: DivisibleByOperator,
      trueTarget: TargetMonkey,
      falseTarget: TargetMonkey
  )

  object MoneyOperation:
    def fromRaw(opStr: String, opAmountStr: String): MoneyOperation =
      val op = opStr match {
        case "+" => Operation.Plus
        case "*" => Operation.Mult
        case _   => throw IllegalArgumentException(s"unknown operator: $opStr")
      }
      val amount = opAmountStr match {
        case "old"                                 => None
        case amount if Try(amount.toInt).isSuccess => Some(amount.toInt)
        case _ => throw IllegalArgumentException(s"unknown amount: $opAmountStr")
      }
      MoneyOperation(op, amount)

  case class Monkey(id: Int, items: List[Int], operation: MoneyOperation, test: TestOperator)

  object Monkey:
    private val idPattern: Regex          = """^Monkey\s(\d):$""".r
    private val itemsPattern: Regex       = """^\s+Starting\sitems:\s([\d,\s]*)""".r
    private val opPattern: Regex          = """^\s+Operation:\snew\s=\sold\s([*+])\s([old\d]+)$""".r
    private val testPattern: Regex        = """^\s+Test:\sdivisible\sby\s(\d+)$""".r
    private val trueTargetPattern: Regex  = """^\s+If\strue:\sthrow\sto\smonkey\s(\d+)$""".r
    private val falseTargetPattern: Regex = """^\s+If\sfalse:\sthrow\sto\smonkey\s(\d+)$""".r

    def fromRaw(input: String): Monkey =
      val lines = input.split("\n")

      val idRow = lines(0)
      val id: Int = idRow match {
        case idPattern(idStr) if Try(idStr.toInt).isSuccess => idStr.toInt
        case _ => throw IllegalAccessException(s"Cant parse id row '$idRow'")
      }

      val itemsRow                           = lines(1)
      val parseItemsCsv: String => List[Int] = s => s.split(",").map(_.trim.toInt).toList
      val items: List[Int] = itemsRow match {
        case itemsPattern(itemsCsv) if Try(parseItemsCsv(itemsCsv)).isSuccess =>
          parseItemsCsv(itemsCsv)
        case _ => throw IllegalAccessException(s"Cant parse items row '$itemsRow'")
      }

      val opRow = lines(2)
      val op: MoneyOperation = opRow match {
        case opPattern(op, opNum) => MoneyOperation.fromRaw(op, opNum)
        case _                    => throw IllegalAccessException(s"Cant parse op row '$opRow'")
      }

      val testRow = lines(3)
      val divOp: DivisibleByOperator = testRow match {
        case testPattern(denumStr) => DivisibleByOperator(denumStr.toInt)
        case _ => throw IllegalAccessException(s"Cant parse test row '$testRow'")
      }

      val testTrueRow = lines(4)
      val trueTarget: TargetMonkey = testTrueRow match {
        case trueTargetPattern(id) => TargetMonkey(id.toInt)
        case _ => throw IllegalAccessException(s"Cant parse true target monkey row '$testTrueRow'")
      }

      val testFalseRow = lines(5)
      val falseTarget: TargetMonkey = testFalseRow match {
        case falseTargetPattern(id) => TargetMonkey(id.toInt)
        case _ =>
          throw IllegalAccessException(s"Cant parse false target monkey row '$testFalseRow'")
      }

      Monkey(id, items, op, TestOperator(divOp, trueTarget, falseTarget))

  def parseFile(): IO[Seq[Monkey]] = for {
    input <- inputResource.use(src => readLines(src))
    inputByMonkey = input.split("\n\n").map(Monkey.fromRaw)
  } yield inputByMonkey.toSeq

  override def run = for {
    input <- parseFile()
    _     <- IO(println(input))
  } yield ()
}
