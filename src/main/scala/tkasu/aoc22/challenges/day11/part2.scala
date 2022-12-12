package tkasu.aoc22.challenges.day11

import cats.effect.{IO, IOApp}
import tkasu.aoc22.challenges.day11.part1.Monkey
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.util.Try
import scala.util.matching.Regex

object part2 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day11/part1/input")

  enum Operation:
    case Plus, Mult

  case class MonkeyOperator(op: Operation, amount: Option[Int]) {
    def exec(item: BigInt): BigInt = this match {
      case MonkeyOperator(Operation.Plus, Some(amount)) => item + amount
      case MonkeyOperator(Operation.Plus, None)         => item + item
      case MonkeyOperator(Operation.Mult, Some(amount)) => item * amount
      case MonkeyOperator(Operation.Mult, None)         => item * item
    }
  }

  case class DivisibleByOperator(denum: Int) {
    def testItem(item: BigInt): Boolean = item % denum == 0
  }
  case class TargetMonkey(id: Int)
  case class TestOperator(
      op: DivisibleByOperator,
      trueTarget: TargetMonkey,
      falseTarget: TargetMonkey
  ) {
    def testItem(item: BigInt): Boolean = op.testItem(item)
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
      items: List[BigInt],
      operation: MonkeyOperator,
      test: TestOperator,
      inspectedItems: BigInt
  ) {
    def playRound(denumLcm: BigInt): (Monkey, Map[TargetMonkey, List[BigInt]]) =
      items.foldLeft[(Monkey, Map[TargetMonkey, List[BigInt]])](this, Map.empty) {
        case ((monkeyState, monkeysToThrow), nextItem) =>
          val newItemRes      = operation.exec(nextItem)
          val testRes = test.testItem(newItemRes)
          val newItem = newItemRes % denumLcm
          val targetMonkey = if (testRes) test.trueTarget else test.falseTarget
          val newMonkeyState = monkeyState.copy(
            items = monkeyState.items.tail,
            inspectedItems = monkeyState.inspectedItems + 1
          )
          val newMonkeysToThrow: Map[TargetMonkey, List[BigInt]] =
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

      val itemsRow = lines(1)
      val parseItemsCsv: String => List[BigInt] = s =>
        s.split(",").map(_.trim.toInt).map(value => BigInt(value)).toList
      val items: List[BigInt] = itemsRow match {
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

  def playRound(monkeys: List[Monkey], denumLcm: BigInt): List[Monkey] =
    monkeys.indices.foldLeft(monkeys) { case (monkeysState, nextMonkeyIdx) =>
      val (newMonkeyState, monkeysToThrow) = monkeysState(nextMonkeyIdx).playRound(denumLcm)
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
  def playRounds(monkeys: List[Monkey], rounds: Int, denumLcm: BigInt, debug: Boolean): IO[List[Monkey]] =
    if (rounds == 0) IO(monkeys)
    else
      if (debug) println(s"Executing round $rounds")
      playRounds(playRound(monkeys, denumLcm), rounds = rounds - 1, denumLcm, debug)

  def getMonkeyBusiness(monkeys: List[Monkey]): BigInt =
    monkeys
      .map(_.inspectedItems)
      .sorted(Ordering[BigInt].reverse)
      .take(2)
      .product


  def calcGcf(n1: BigInt, n2: BigInt): BigInt =
    (BigInt(1) to List(n1, n2).min).filter(factor => (n1 % factor == 0) && (n2 % factor == 0)).last

  def calcLcm(values: Int*): BigInt =
    values.map(BigInt(_)).reduce { case (acc, next) =>
      val gcf = calcGcf(acc, next)
      acc  * next / gcf
    }

  def lowestCommonMultipleDenum(monkeys: List[Monkey]): BigInt =
    val denums = monkeys.map(_.test.op.denum)
    calcLcm(denums: _*)

  def parseFile(): IO[List[Monkey]] = for {
    input <- inputResource.use(src => readLines(src))
    inputByMonkey = input.split("\n\n").map(Monkey.fromRaw)
  } yield inputByMonkey.toList

  override def run = for {
    monkeys <- parseFile()
    lcm = lowestCommonMultipleDenum(monkeys)
    _ <- IO(println(s"Lowest Common Multiple: $lcm"))
    monkeysAfterRounds <- playRounds(monkeys, 10000, denumLcm = lcm, false)
     //_ <- IO(println(monkeysAfterRounds))
    _ <- IO(println(s"Monkey Business: ${getMonkeyBusiness(monkeysAfterRounds)}"))
  } yield ()
}
