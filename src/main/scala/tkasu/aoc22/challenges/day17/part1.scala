package tkasu.aoc22.challenges.day17

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}
import tkasu.aoc22.utils.matrix._

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec

object part1 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day17/part1/input_example")
  private val inputResource = makeSourceResource("day17/part1/input")

  case class Chamber(
      rocks: Seq[Rock],
      highestRock: Int,
      jetPattern: Array[JetDirection],
      jetIdx: Int
  ):
    val width: Int = 7

    @tailrec
    private def dropUntilRest(rock: Rock, jetIdx: Int): (Rock, Int) =
      val (jet, nextJetIdx) = Try(jetPattern(jetIdx)) match {
        case Success(nextJet) => (nextJet, jetIdx + 1)
        case Failure(_)       => (jetPattern(0), 1)
      }
      val jetShiftedRock = rock.applyJet(jet, this)
      val rockMovedDown  = jetShiftedRock.moveDown()
      if (rocks.exists(existingRock => Rock.rocksOverlap(rockMovedDown, existingRock))) {
        (jetShiftedRock, nextJetIdx)
      } else if (rockMovedDown.locations.exists(_.row < 1)) {
        (jetShiftedRock, nextJetIdx)
      } else {
        dropUntilRest(rockMovedDown, nextJetIdx)
      }

    def dropRock(): Chamber =
      val nextRockColumn = 2
      val rock: Rock = rocks.size % 5 match {
        case 0 => MinusRock(Location(row = highestRock + 4, column = nextRockColumn))
        case 1 => PlusRock(Location(row = highestRock + 6, column = nextRockColumn))
        case 2 => CornerRock(Location(row = highestRock + 6, column = nextRockColumn))
        case 3 => TowerRock(Location(row = highestRock + 7, column = nextRockColumn))
        case 4 => SquareRock(Location(row = highestRock + 5, column = nextRockColumn))
      }

      val (droppedRock, nextJetIdx) = dropUntilRest(rock, jetIdx)
      val rockHeight                = droppedRock.locations.map(_.row).max
      this.copy(
        rocks = rocks :+ droppedRock,
        highestRock = highestRock.max(rockHeight),
        jetIdx = nextJetIdx
      )

    def rockMatrix: Array[Array[Char]] =
      Array.ofDim[Option[Seq[Int]]](highestRock + 3, width).zipWithIndex.map { case (row, rowIdx) =>
        row.indices.map { columnIdx =>
          val curLoc = Location(rowIdx, columnIdx)
          if (rowIdx == 0) '-'
          else if (rocks.exists(_.locations.contains(curLoc))) '#'
          else '.'
        }.toArray
      }

    def stringifiedRockMatrix =
      stringifyMatrixNoSep(rockMatrix.reverse, "Chamber")

  enum JetDirection:
    case Left, Right

  object JetDirection:
    def fromRaw(c: Char): JetDirection =
      c match {
        case '<' => JetDirection.Left
        case '>' => JetDirection.Right
      }

  case class Location(row: Int, column: Int)

  sealed trait Rock:
    val cornerLoc: Location
    val locations: Set[Location]
    lazy val minCol = locations.map(_.column).min
    lazy val maxCol = locations.map(_.column).max

    def moveDown(): Rock =
      val newLoc = this.cornerLoc.copy(row = this.cornerLoc.row - 1)
      this match {
        case MinusRock(_)  => MinusRock(newLoc)
        case PlusRock(_)   => PlusRock(newLoc)
        case CornerRock(_) => CornerRock(newLoc)
        case TowerRock(_)  => TowerRock(newLoc)
        case SquareRock(_) => SquareRock(newLoc)
      }

    def applyJet(jet: JetDirection, chamber: Chamber): Rock =
      def newLocation(jet: JetDirection): Option[Location] =
        val shift = jet match {
          case JetDirection.Left  => -1
          case JetDirection.Right => +1
        }
        // TODO optimze
        lazy val shiftedLocs = this.locations.map(loc => loc.copy(column = loc.column + shift))
        lazy val overlapsWithRocks = chamber.rocks
          .flatMap(_.locations)
          .toSet
          .intersect(shiftedLocs)
          .nonEmpty
        if (minCol + shift < 0 || maxCol + shift >= chamber.width) None
        else if (overlapsWithRocks) None
        else Some(cornerLoc.copy(column = cornerLoc.column + shift))

      newLocation(jet) match {
        case None => this
        case Some(loc) =>
          this match {
            case MinusRock(_)  => MinusRock(loc)
            case PlusRock(_)   => PlusRock(loc)
            case CornerRock(_) => CornerRock(loc)
            case TowerRock(_)  => TowerRock(loc)
            case SquareRock(_) => SquareRock(loc)
          }
      }

  object Rock:
    def rocksOverlap(r1: Rock, r2: Rock): Boolean =
      // TODO, optimize based on cornerLoc distance
      r1.locations.intersect(r2.locations).nonEmpty

  case class MinusRock(cornerLoc: Location) extends Rock:
    val locations = Set(
      cornerLoc,
      cornerLoc.copy(column = cornerLoc.column + 1),
      cornerLoc.copy(column = cornerLoc.column + 2),
      cornerLoc.copy(column = cornerLoc.column + 3)
    )

  case class PlusRock(cornerLoc: Location) extends Rock:
    val locations = Set(
      cornerLoc.copy(column = cornerLoc.column + 1),                          // top
      cornerLoc.copy(row = cornerLoc.row - 1),                                // mid left
      cornerLoc.copy(row = cornerLoc.row - 1, column = cornerLoc.column + 1), // mid mid
      cornerLoc.copy(row = cornerLoc.row - 1, column = cornerLoc.column + 2), // mid right
      cornerLoc.copy(row = cornerLoc.row - 2, column = cornerLoc.column + 1)  // bottom mid
    )

  case class CornerRock(cornerLoc: Location) extends Rock:
    val locations = Set(
      cornerLoc.copy(column = cornerLoc.column + 2),                          // top right
      cornerLoc.copy(row = cornerLoc.row - 1, column = cornerLoc.column + 2), // mid right
      cornerLoc.copy(row = cornerLoc.row - 2),                                // bottom left
      cornerLoc.copy(row = cornerLoc.row - 2, column = cornerLoc.column + 1), // bottom mid
      cornerLoc.copy(row = cornerLoc.row - 2, column = cornerLoc.column + 2)  // bottom right
    )

  case class TowerRock(cornerLoc: Location) extends Rock:
    val locations = Set(
      cornerLoc,
      cornerLoc.copy(row = cornerLoc.row - 1),
      cornerLoc.copy(row = cornerLoc.row - 2),
      cornerLoc.copy(row = cornerLoc.row - 3)
    )

  case class SquareRock(cornerLoc: Location) extends Rock:
    val locations = Set(
      cornerLoc,                                                             // top left
      cornerLoc.copy(column = cornerLoc.column + 1),                         // top right
      cornerLoc.copy(row = cornerLoc.row - 1),                               // mid left
      cornerLoc.copy(row = cornerLoc.row - 1, column = cornerLoc.column + 1) // mid right
    )

  @tailrec
  def dropN(chamber: Chamber, rocks: Int): Chamber =
    if (rocks == 0) chamber
    else
      val newChaimber = chamber.dropRock()
      dropN(newChaimber, rocks - 1)

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    jetDirs = input.toCharArray.map(JetDirection.fromRaw)
  } yield jetDirs

  override def run = for {
    jetDirs <- parseFile()
    // _       <- IO(println(jetDirs.toSeq))
    initialChamber = Chamber(
      rocks = Seq.empty,
      highestRock = 0,
      jetPattern = jetDirs,
      jetIdx = 0
    )
    chamber = dropN(initialChamber, 2022)
    _ <- IO(println(chamber.stringifiedRockMatrix))
    _ <- IO(println(chamber.highestRock))
  } yield ()
}
