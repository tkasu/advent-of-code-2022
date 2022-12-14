package tkasu.aoc22.challenges.day11

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

import scala.annotation.tailrec
import scala.util.Try
import scala.util.matching.Regex

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day11/part1/input")

  enum Operation:
    case Plus, Mult

  case class MonkeyOperator(op: Operation, amount: Option[Int]) {
    def exec(item: Int): Int = this match {
      case MonkeyOperator(Operation.Plus, Some(amount)) => item + amount
      case MonkeyOperator(Operation.Plus, None)         => item + item
      case MonkeyOperator(Operation.Mult, Some(amount)) => item * amount
      case MonkeyOperator(Operation.Mult, None)         => item * item
    }
  }

  case class DivisibleByOperator(denum: Int) {
    def testItem(item: Int): Boolean = item % denum == 0
  }
  case class TargetMonkey(id: Int)
  case class TestOperator(
      op: DivisibleByOperator,
      trueTarget: TargetMonkey,
      falseTarget: TargetMonkey
  ) {
    def testItem(item: Int): Boolean = op.testItem(item)
  }

  object MonkeyOperator:
    def fromRaw(opStr: String, opAmountStr: String): MonkeyOperator =
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
      MonkeyOperator(op, amount)

  case class Monkey(
      id: Int,
      items: List[Int],
      operation: MonkeyOperator,
      test: TestOperator,
      inspectedItems: Int
  ) {
    def playRound(): (Monkey, Map[TargetMonkey, List[Int]]) =
      items.foldLeft[(Monkey, Map[TargetMonkey, List[Int]])](this, Map.empty) {
        case ((monkeyState, monkeysToThrow), nextItem) =>
          val newItem      = operation.exec(nextItem) / 3
          val targetMonkey = if (test.testItem(newItem)) test.trueTarget else test.falseTarget
          val newMonkeyState = monkeyState.copy(
            items = monkeyState.items.tail,
            inspectedItems = monkeyState.inspectedItems + 1
          )
          val newMonkeysToThrow: Map[TargetMonkey, List[Int]] =
            monkeysToThrow.get(targetMonkey) match {
              case None        => monkeysToThrow + (targetMonkey -> List(newItem))
              case Some(items) => monkeysToThrow + (targetMonkey -> (items :+ newItem))
            }
          (newMonkeyState, newMonkeysToThrow)
      }
  }

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
      val op: MonkeyOperator = opRow match {
        case opPattern(op, opNum) => MonkeyOperator.fromRaw(op, opNum)
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

      Monkey(id, items, op, TestOperator(divOp, trueTarget, falseTarget), inspectedItems = 0)

  def playRound(monkeys: List[Monkey]): List[Monkey] =
    monkeys.indices.foldLeft(monkeys) { case (monkeysState, nextMonkeyIdx) =>
      val (newMonkeyState, monkeysToThrow) = monkeysState(nextMonkeyIdx).playRound()
      val monkeysStateWithThrown = monkeysToThrow.foldLeft(monkeysState) {
        case (monkeysStateUpdate, (nextTargetMonkey, itemsToThrow)) =>
          val targetMonkey = monkeysStateUpdate(nextTargetMonkey.id)
          val newTargetMonkey = targetMonkey.copy(
            items = targetMonkey.items ++ itemsToThrow
          )
          monkeysStateUpdate.updated(nextTargetMonkey.id, newTargetMonkey)
      }

      monkeysStateWithThrown
        .updated(nextMonkeyIdx, newMonkeyState)
    }

  @tailrec
  def playRounds(monkeys: List[Monkey], rounds: Int): List[Monkey] =
    if (rounds == 0) monkeys
    else playRounds(playRound(monkeys), rounds = rounds - 1)

  def getMonkeyBusiness(monkeys: List[Monkey]): Int =
    monkeys
      .map(_.inspectedItems)
      .sorted(Ordering[Int].reverse)
      .take(2)
      .product

  def parseFile(): IO[List[Monkey]] = for {
    input <- inputResource.use(src => readLines(src))
    inputByMonkey = input.split("\n\n").map(Monkey.fromRaw)
  } yield inputByMonkey.toList

  override def run = for {
    monkeys <- parseFile()
    // _       <- IO(println(monkeys))
    monkeysAfterRounds = playRounds(monkeys, 20)
    // _ <- IO(println(monkeysAfterRounds))
    _ <- IO(println(s"Monkey Business: ${getMonkeyBusiness(monkeysAfterRounds)}"))
  } yield ()
}
