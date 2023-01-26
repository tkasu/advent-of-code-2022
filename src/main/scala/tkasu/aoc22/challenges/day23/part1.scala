package tkasu.aoc22.challenges.day23

import scala.annotation.tailrec
import cats.effect.{IO, IOApp}
import tkasu.aoc22.challenges.day23.part1.Direction.North
import tkasu.aoc22.challenges.day23.part1.State.{
  calcHeight,
  calcMinCol,
  calcMinRow,
  calcWidth,
  getCharMatrix
}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}
import tkasu.aoc22.utils.matrix.*

object part1 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day23/part1/input_example_2")
  private val inputResource = makeSourceResource("day23/part1/input")

  val DIR_CHECK_MAP: Map[Direction, Set[Direction]] =
    Map(
      Direction.North -> Set(Direction.North, Direction.NorthEast, Direction.NorthWest),
      Direction.South -> Set(Direction.South, Direction.SouthEast, Direction.SouthWest),
      Direction.West  -> Set(Direction.West, Direction.NorthWest, Direction.SouthWest),
      Direction.East  -> Set(Direction.East, Direction.NorthEast, Direction.SouthEast)
    )

  case class State(
      elves: Set[Location],
      movePrio: List[Direction] =
        List(Direction.North, Direction.South, Direction.West, Direction.East)
  ):

    lazy val height = calcHeight(elves)
    lazy val width  = calcWidth(elves)

    lazy val charMatrix: Array[Array[Char]] = getCharMatrix(elves)

    lazy val nearbyElvesMap: Map[Location, Set[Direction]] =
      val allDirs = Direction.values
      elves.map { elf =>
        val dirsWithELf = allDirs
          .filter(dir => State.elfIndDir(elf, dir, elves))
        elf -> dirsWithELf.toSet
      }.toMap

    lazy val lonelyElves: Set[Location] =
      nearbyElvesMap
        .filter { case (_, value) => value.isEmpty }
        .map { case (key, _) => key }
        .toSet

    def emptyGrids(): Int =
      charMatrix.flatten.count(_ == '.')

    @tailrec
    final def moveElvesNTimes(times: Int, debug: Boolean = false): State =
      if (debug) println(this)
      if (times <= 0) this
      else this.moveElves().moveElvesNTimes(times - 1, debug)

    def moveElves(): State =
      val proposedMoves = getProposedMoves()
      val duplicateLocs = proposedMoves.values.flatten
        .groupBy(identity)
        .filter((_, locs) => locs.size > 1)
        .keys
        .toSet
      val elfMoves = proposedMoves
        .filter((_, targetLoc) =>
          targetLoc match {
            case None         => false
            case Some(target) => !duplicateLocs.contains(target)
          }
        )
        .map((k, v) => k -> v.get)
      val newElves = Set.from(elfMoves.values ++ elves.diff(elfMoves.keys.toSet))
      State(newElves, movePrio.tail :+ movePrio.head)

    def getProposedMoves(): Map[Location, Option[Location]] =
      val elvesToMove = elves.diff(lonelyElves)
      elvesToMove.map { elf =>
        val moveDir = movePrio.find { dir =>
          DIR_CHECK_MAP(dir).forall(checkDir => !State.elfIndDir(elf, checkDir, elves))
        }
        elf -> moveDir.map(elf.getDirLoc)
      }.toMap

    override def toString =
      stringifyMatrixNoSep(charMatrix, "State")

  object State:

    def elfIndDir(elf: Location, dir: Direction, elves: Set[Location]): Boolean =
      val searchDir = elf.getDirLoc(dir)
      elves.contains(searchDir)

    def calcMinCol(elves: Set[Location]) = elves.map(_.column).min

    def calcMinRow(elves: Set[Location]) = elves.map(_.row).min

    def calcWidth(elves: Set[Location]) =
      val minColumn = calcMinCol(elves)
      val maxColumn = elves.map(_.column).max
      maxColumn - minColumn + 1

    def calcHeight(elves: Set[Location]) =
      val minRow = calcMinRow(elves)
      val maxRow = elves.map(_.row).max
      maxRow - minRow + 1

    def getCharMatrix(elves: Set[Location]): Array[Array[Char]] =
      val minRow    = calcMinRow(elves)
      val minColumn = calcMinCol(elves)
      val height    = calcHeight(elves)
      val width     = calcWidth(elves)

      Array.ofDim[Char](height, width).zipWithIndex.map { case (row, rowIdx) =>
        row.zipWithIndex.map { case (_, colIdx) =>
          if (elves.contains(Location(rowIdx + minRow, colIdx + minColumn))) '#'
          else '.'
        }
      }

    def fromRaw(s: String): State =
      val charMatrix = s.split("\n").map(_.toCharArray)
      val elves = charMatrix.zipWithIndex.flatMap { case (row, rowIdx) =>
        row.zipWithIndex.flatMap { case (char, colIdx) =>
          char match {
            case '#' => Some(Location(rowIdx, colIdx))
            case '.' => None
          }
        }
      }
      State(elves.toSet)

  case class Location(row: Int, column: Int):

    def getDirLoc(dir: Direction): Location =
      dir match {
        case Direction.North     => this.copy(row = row - 1)
        case Direction.South     => this.copy(row = row + 1)
        case Direction.East      => this.copy(column = column + 1)
        case Direction.West      => this.copy(column = column - 1)
        case Direction.NorthWest => this.getDirLoc(Direction.North).getDirLoc(Direction.West)
        case Direction.NorthEast => this.getDirLoc(Direction.North).getDirLoc(Direction.East)
        case Direction.SouthWest => this.getDirLoc(Direction.South).getDirLoc(Direction.West)
        case Direction.SouthEast => this.getDirLoc(Direction.South).getDirLoc(Direction.East)
      }

  enum Direction:
    case North, East, South, West, NorthEast, SouthEast, NorthWest, SouthWest

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    state = State.fromRaw(input)
  } yield state

  override def run = for {
    initialState <- parseFile()
    endState = initialState.moveElvesNTimes(10, debug = true)
    _ <- IO(println(s"Count of empty grids: ${endState.emptyGrids()}"))
  } yield ()
}
