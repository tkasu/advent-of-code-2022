package tkasu.aoc22.challenges.day15

import cats.implicits.*
import cats.effect.{Deferred, IO, IOApp}
import tkasu.aoc22.utils.string.*
import tkasu.aoc22.utils.files.{closeFile, makeSourceResource, readLines}
import tkasu.aoc22.challenges.day15.part1.{Location, Sensor, canHaveBeacon, parseFile}

import scala.util.matching.Regex
import scala.util.control.NonLocalReturns.*

object part2 extends IOApp.Simple {

  val MATRIX_SIZE_LIMIT = 4_000_000

  object SensorExtensions {
    extension (sensor: Sensor) {

      def diamondDimensions(): DiamondDimensions =
        DiamondDimensions(
          up = Location(sensor.loc.row - sensor.absDist, sensor.loc.column),
          down = Location(sensor.loc.row + sensor.absDist, sensor.loc.column),
          left = Location(sensor.loc.row, sensor.loc.column - sensor.absDist),
          right = Location(sensor.loc.row, sensor.loc.column + sensor.absDist)
        )

      def triangleDimensions(): OutsideTriangles =
        val diamond = sensor.diamondDimensions()
        OutsideTriangles(
          upLeft = TriangleUpLeft(
            downLeft = diamond.left,
            upLeft = Location(diamond.up.row, diamond.left.column),
            upRight = diamond.up
          ),
          upRight = TriangleUpRight(
            downRight = diamond.right,
            upRight = Location(diamond.up.row, diamond.right.column),
            upLeft = diamond.up
          ),
          downLeft = TriangleDownLeft(
            upLeft = diamond.left,
            downLeft = Location(diamond.down.row, diamond.left.column),
            downRight = diamond.down
          ),
          downRight = TriangleDownRight(
            upRight = diamond.right,
            downRight = Location(diamond.down.row, diamond.right.column),
            downLeft = diamond.down
          )
        )
    }
  }

  import SensorExtensions._

  case class DiamondDimensions(up: Location, down: Location, left: Location, right: Location)

  trait Triangle:
    def shadowedBy(sensor: Sensor): Boolean
    def findPossibleBeacon(sensors: Seq[Sensor]): Option[Location]

    def canHaveBeacon(loc: Location, sensors: Seq[Sensor]): Boolean =
      if (!(sensors.forall(sensor => sensor.loc != loc && sensor.closestBeacon != loc))) false
      else
        sensors.forall { sensor =>
          val dist = Sensor.calcAbsoluteDist(sensor.loc, loc)
          dist > sensor.absDist
        }

  case class TriangleUpLeft(upLeft: Location, downLeft: Location, upRight: Location)
      extends Triangle:

    override def shadowedBy(sensor: Sensor): Boolean =
      val diamond = sensor.diamondDimensions()
      if (upLeft.row < (diamond.up.row + ((diamond.down.row - diamond.up.row) / 4))) false
      else if (upLeft.row > (diamond.down.row - ((diamond.down.row - diamond.up.row) / 4))) false
      else if (
        upLeft.column < (diamond.left.column + ((diamond.right.column - diamond.left.column) / 4))
      ) false
      else if (upRight.column > diamond.right.column) false
      else true

    override def findPossibleBeacon(sensors: Seq[Sensor]) =
      val yMin = Math.max(upLeft.row, 0)
      val yMax = Math.max(Math.min(downLeft.row, MATRIX_SIZE_LIMIT), 0)
      val xMin = Math.max(upLeft.column, 0)
      val xMax = Math.max(Math.min(upRight.column, MATRIX_SIZE_LIMIT), 0)
      returning {
        for (rowIdx <- yMin to yMax) do
          for (columnIdx <- xMin to xMax - (rowIdx - yMin)) do
            val loc = Location(rowIdx, columnIdx)
            if (canHaveBeacon(loc, sensors))
              println(addThreadPrefix(s"FOUND RESULT: $loc"))
              throwReturn(Some(loc))

        println(
          addThreadPrefix(
            s"Did not find result from $this between rows: $yMin & $yMax"
          )
        )
        None
      }

  case class TriangleUpRight(upLeft: Location, downRight: Location, upRight: Location)
      extends Triangle:

    override def shadowedBy(sensor: Sensor): Boolean =
      val diamond = sensor.diamondDimensions()
      if (upLeft.row < (diamond.up.row + ((diamond.down.row - diamond.up.row) / 4))) false
      else if (upLeft.row > (diamond.down.row - ((diamond.down.row - diamond.up.row) / 4))) false
      else if (upLeft.column < diamond.left.column) false
      else if (
        upRight.column > (diamond.right.column - ((diamond.right.column - diamond.left.column) / 4))
      ) false
      else true

    override def findPossibleBeacon(sensors: Seq[Sensor]) =
      val yMin = Math.max(upLeft.row, 0)
      val yMax = Math.max(Math.min(downRight.row, MATRIX_SIZE_LIMIT), 0)
      val xMax = Math.max(Math.min(upRight.column, MATRIX_SIZE_LIMIT), 0)
      returning {
        for (rowIdx <- yMin to yMax) do
          for (columnIdx <- (xMax - (rowIdx - yMin)) to xMax) do
            val loc = Location(rowIdx, columnIdx)
            if (canHaveBeacon(loc, sensors))
              println(addThreadPrefix(s"FOUND RESULT: $loc"))
              throwReturn(Some(loc))

        println(
          addThreadPrefix(
            s"Did not find result from $this between rows: $yMin & $yMax"
          )
        )
        None
      }

  case class TriangleDownLeft(upLeft: Location, downLeft: Location, downRight: Location)
      extends Triangle:

    override def shadowedBy(sensor: Sensor): Boolean =
      val diamond = sensor.diamondDimensions()
      if (upLeft.row < diamond.up.row) false
      else if (upLeft.row < (diamond.up.row + ((diamond.down.row - diamond.up.row) / 4))) false
      else if (
        upLeft.column < (diamond.left.column + ((diamond.right.column - diamond.left.column) / 4))
      ) false
      else if (
        downRight.column > (diamond.right.column - ((diamond.right.column - diamond.left.column) / 4))
      ) false
      else true

    override def findPossibleBeacon(sensors: Seq[Sensor]) =
      val yMin = Math.max(upLeft.row, 0)
      val yMax = Math.max(Math.min(downLeft.row, MATRIX_SIZE_LIMIT), 0)
      val xMin = Math.max(downLeft.column, 0)
      val xMax = Math.max(downRight.column, 0)
      returning {
        for (rowIdx <- yMin to yMax) do
          val rowXMax = xMin + xMax - (xMax - (rowIdx - yMin))
          for (columnIdx <- xMin to rowXMax) do
            val loc = Location(rowIdx, columnIdx)
            if (canHaveBeacon(loc, sensors))
              println(addThreadPrefix(s"FOUND RESULT: $loc"))
              throwReturn(Some(loc))

        println(
          addThreadPrefix(
            s"Did not find result from $this between rows: $yMin & $yMax"
          )
        )
        None
      }

  case class TriangleDownRight(downLeft: Location, downRight: Location, upRight: Location)
      extends Triangle:
    override def shadowedBy(sensor: Sensor): Boolean =

      val diamond = sensor.diamondDimensions()
      if (downLeft.row < (diamond.up.row + ((diamond.down.row - diamond.up.row) / 4))) false
      else if (downLeft.row > diamond.down.row) false
      else if (
        downLeft.column < (diamond.left.column + ((diamond.right.column - diamond.left.column) / 4))
      ) false
      else if (
        downRight.column > (diamond.right.column - ((diamond.right.column - diamond.left.column) / 4))
      ) false
      else true

    override def findPossibleBeacon(sensors: Seq[Sensor]) =
      val yMin = Math.max(upRight.row, 0)
      val yMax = Math.min(downLeft.row, MATRIX_SIZE_LIMIT)
      val xMax = Math.min(downRight.column, MATRIX_SIZE_LIMIT)
      returning {
        for (rowIdx <- yMin to yMax) do
          val rowXMin = xMax - (rowIdx - yMin)
          for (columnIdx <- rowXMin to xMax) do
            val loc = Location(rowIdx, columnIdx)
            if (canHaveBeacon(loc, sensors))
              println(addThreadPrefix(s"FOUND RESULT: $loc"))
              throwReturn(Some(loc))

        println(
          addThreadPrefix(
            s"Did not find result from $this between rows: $yMin & $yMax."
          )
        )
        None
      }

  case class OutsideTriangles(
      upLeft: TriangleUpLeft,
      upRight: TriangleUpRight,
      downLeft: TriangleDownLeft,
      downRight: TriangleDownRight
  )

  def calcTuningFrequency(loc: Location): BigInt =
    BigInt(loc.column) * 4_000_000 + loc.row

  def findUnreachable(
      triangle: Triangle,
      sensors: Seq[Sensor],
      result: Deferred[IO, Location]
  ): IO[Boolean] =
    for {
      _ <- IO.unit
      findIOFib <- IO(triangle.findPossibleBeacon(sensors)).flatMap {
        case Some(loc) => result.complete(loc)
        case None      => IO.unit
      }.start
      outcome <- IO.racePair(result.get >> findIOFib.cancel, findIOFib.joinWithNever)
      threadFoundRes = outcome.isLeft
    } yield threadFoundRes

  def outerTrianglesNotCovered(sensors: Seq[Sensor]) =
    val triangles = List(
      sensors.map(_.triangleDimensions().upLeft),
      sensors.map(_.triangleDimensions().upRight),
      sensors.map(_.triangleDimensions().downLeft),
      sensors.map(_.triangleDimensions().downRight)
    ).flatten
    triangles.filterNot(triangle => sensors.exists(sensor => triangle.shadowedBy(sensor)))

  /** DISCLAIMER: I'm pretty sure that there is bugs in this implementation, but it managed to trim
    * the amount of scanned cells to low enough. It still took ~ 3 HOURS to get the solution with
    * Macbook Air M1 8GB with 8 cores running red for the whole time. And there seems to be some
    * problem with the stopping logic, need to revisit that So I'm pretty sure that there is A LOT
    * better solution for this, but I want to move forward with AoC. To be improved later (maybe,
    * probably not),
    */
  override def run = for {
    sensors   <- parseFile()
    resSignal <- Deferred[IO, Location]
    trianglesNotShadowed = outerTrianglesNotCovered(sensors)
    _ <- IO(println(trianglesNotShadowed))
    _ <- IO(println(trianglesNotShadowed.size))
    findJobs = trianglesNotShadowed.map(triangle => findUnreachable(triangle, sensors, resSignal))
    foundResultArr <- findJobs.parSequence
    beacon         <- if (foundResultArr.contains(true)) resSignal.get.map(Some(_)) else IO(None)
    _              <- IO(println(s"Positions where beacon can be present: $beacon"))
    _ <- IO(
      println(s"Tuning frequency: ${calcTuningFrequency(beacon.getOrElse(Location(0, 0)))}")
    )
  } yield ()
}
