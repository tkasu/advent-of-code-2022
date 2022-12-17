package tkasu.aoc22.challenges.day14

import scala.annotation.tailrec
import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}
import tkasu.aoc22.utils.matrix._

object part1 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day14/part1/input_example")
  private val inputResource = makeSourceResource("day14/part1/input")

  enum Marker:
    case Rock, Air, Sand, SandSource

    def toChar: Char = this match {
      case Marker.Rock       => '#'
      case Marker.Air        => '.'
      case Marker.Sand       => 'o'
      case Marker.SandSource => '+'
    }

  enum Direction:
    case Left, Right, Up, Down, DownRight, DownLeft

  case class Location(row: Int, column: Int):

    def move(dir: Direction): Location =
      // Up/Down directions has been reversed in this problem
      dir match {
        case Direction.Down      => Location(this.row + 1, this.column)
        case Direction.DownRight => Location(this.row + 1, this.column + 1)
        case Direction.DownLeft  => Location(this.row + 1, this.column - 1)
        case _ => throw IllegalArgumentException(s"Move direction $dir is not supported yet.")
      }

  object Location:
    def fromRaw(s: String): Location =
      val fst :: snd :: Nil = s.split(",").toList
      Location(snd.toInt, fst.toInt)

  case class Vector(from: Location, to: Location):

    def toLocations: Seq[Location] =
      val minRow    = List(from.row, to.row).min
      val maxRow    = List(from.row, to.row).max
      val minColumn = List(from.column, to.column).min
      val maxColumn = List(from.column, to.column).max

      if (minRow == maxRow)
        for column <- (minColumn to maxColumn) yield Location(minRow, column)
      else if (minColumn == maxColumn)
        for row <- (minRow to maxRow) yield Location(row, minColumn)
      else
        throw IllegalArgumentException(s"Diagonal vectors are not supported, found $this")

  object Vector:
    def fromRawToVectors(line: String): List[Vector] =
      val locStrings = line.split(" -> ")
      val firstLoc   = Location.fromRaw(locStrings.head)
      locStrings.tail
        .foldLeft[(Location, List[Vector])]((firstLoc, List.empty)) {
          case ((prevLoc, vectors), nextLocRaw) =>
            val nextLoc = Location.fromRaw(nextLocRaw)
            val newVec  = Vector(prevLoc, nextLoc)
            (nextLoc, newVec :: vectors)
        }
        ._2
        .reverse

  case class Cave(rocks: Set[Location], sand: Set[Location]):

    val sandSource = Location(0, 500)

    lazy val width: Int = List(
      rocks.map(_.column).maxOption,
      sand.map(_.column).maxOption
    ).flatten.max + 1

    lazy val height: Int = List(
      rocks.map(_.row).maxOption,
      sand.map(_.row).maxOption
    ).flatten.max + 1

    private lazy val minNonAirWidth = List(
      rocks.map(_.column).minOption,
      sand.map(_.column).minOption
    ).flatten.min

    private def locationInAbyss(loc: Location): Boolean =
      rocks.filter(_.column == loc.column).map(_.row).maxOption match {
        case None          => true
        case Some(rockLoc) => loc.row > rockLoc
      }

    @tailrec
    final def fillSand(debug: Boolean = false): Option[Cave] =
      if (debug) println(this)
      dropSand() match {
        case None => Some(this)
        case Some(cave) => {
          cave.fillSand(debug)
        }
      }

    def dropSand(): Option[Cave] =
      @tailrec
      def getNewLoc(loc: Location, dirs: List[Direction]): Option[Location] =
        dirs match {
          case List() => None
          case head :: _ =>
            val possibleLoc = loc.move(head)
            if (!(sand.contains(possibleLoc) || rocks.contains(possibleLoc))) {
              Some(possibleLoc)
            } else
              getNewLoc(loc, dirs.tail)
        }

      @tailrec
      def getDropLocation(loc: Location, dirs: List[Direction]): Option[Location] =
        getNewLoc(loc, dirs) match {
          case None => Some(loc)
          case Some(newLoc) =>
            if (locationInAbyss(newLoc)) None
            else getDropLocation(newLoc, dirs)
        }

      val initSandLoc  = sandSource
      val moveDirOrder = Seq(Direction.Down, Direction.DownLeft, Direction.DownRight)
      getDropLocation(initSandLoc, moveDirOrder.toList) match {
        case None => None
        case Some(dropLoc) =>
          if (dropLoc == initSandLoc) None
          else Some(this.copy(sand = sand + dropLoc))
      }

    def coordMatrix: Array[Array[Marker]] =
      Array.ofDim[Option[Seq[Int]]](height, width).zipWithIndex.map { case (row, rowIdx) =>
        row.indices.map { columnIdx =>
          val curLoc = Location(rowIdx, columnIdx)
          if (sand.contains(curLoc)) Marker.Sand
          else if (rocks.contains(curLoc)) Marker.Rock
          else if (curLoc == sandSource) Marker.SandSource
          else Marker.Air
        }.toArray
      }

    def coordMatrixForPrint: Array[Array[Char]] =
      coordMatrix
        .map(
          _.slice(minNonAirWidth, width)
        )
        .map(_.map(_.toChar))

    override def toString =
      stringifyMatrixNoSep(coordMatrixForPrint, "Cave")

  object Cave:

    def fromRockVectors(rockVectors: Seq[Vector]): Cave =
      val locations = rockVectors
        .flatMap(_.toLocations)
        .toSet
      Cave(locations, Set.empty)

  def parseFile(): IO[List[Vector]] = for {
    input <- inputResource.use(src => readLines(src))
    rockVectors = input.split("\n").flatMap(Vector.fromRawToVectors).toList
  } yield rockVectors

  override def run = for {
    rockVectors <- parseFile()
    _           <- IO(println(rockVectors))
    cave = Cave.fromRockVectors(rockVectors)
    _ <- IO(println(cave))
    filledCave = cave.fillSand(debug = false)
    _ <- IO(println(filledCave))
    sandOnTopOfTheRocks = filledCave match {
      case None               => 0
      case Some(caveWithSand) => caveWithSand.sand.size
    }
    _ <- IO(println(s"Amount of sand in cave $sandOnTopOfTheRocks"))
  } yield ()
}
