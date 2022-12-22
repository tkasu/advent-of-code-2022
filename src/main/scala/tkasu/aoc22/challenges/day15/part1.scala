package tkasu.aoc22.challenges.day15

import scala.util.matching.Regex
import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

object part1 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day15/part1/input_example")
  private val inputResource = makeSourceResource("day15/part1/input")

  case class Location(row: Int, column: Int)

  case class Sensor(loc: Location, closestBeacon: Location):
    val absDist = Sensor.calcAbsoluteDist(loc, closestBeacon)

  object Sensor:
    private val linePattern: Regex =
      """^Sensor at x=([-\d]+), y=([-\d]+): closest beacon is at x=([-\d]+), y=([-\d]+)$""".r

    def calcAbsoluteDist(from: Location, to: Location): Int =
      (from.row - to.row).abs + (from.column - to.column).abs

    def fromRaw(line: String): Sensor =
      val (sensorX, sensorY, beaconX, beaconY) = line match {
        case linePattern(sX, sY, bX, bY) => (sX.toInt, sY.toInt, bX.toInt, bY.toInt)
        case _ => throw IllegalAccessException(s"Cant parse test row '$line'")
      }
      Sensor(
        Location(sensorY, sensorX),
        Location(beaconY, beaconX)
      )

  /** Get theoretical limits how wide the sensors' reach can be */
  def getTheoreticalWidthLimits(sensors: Seq[Sensor]): (Int, Int) =
    val minMaxReaches =
      sensors
        .map { sensor =>
          (sensor.loc.column - sensor.absDist, sensor.loc.column + sensor.absDist)
        }
    val minReach = minMaxReaches.map(_._1).min
    val maxReach = minMaxReaches.map(_._2).max
    (minReach, maxReach)

  def canHaveBeacon(beacon: Location, sensors: Seq[Sensor]): Boolean =
    sensors
      .forall { sensor =>
        val dist = Sensor.calcAbsoluteDist(sensor.loc, beacon)
        dist > sensor.absDist
      }

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    sensors = input.split("\n").map(Sensor.fromRaw)
  } yield sensors.toSeq

  override def run = for {
    sensors <- parseFile()
    //_       <- IO(println(sensors))
    //_       <- IO(println(sensors.map(_.absDist)))
    widthReachLimits = getTheoreticalWidthLimits(sensors)
    //investigateRow   = 10 // test data
    investigateRow = 2000000  // my data
    investigateLocs = (widthReachLimits._1 to widthReachLimits._2).map(column =>
      Location(investigateRow, column)
    )
    unreachbleLocations  = investigateLocs
      .filter(loc => sensors.forall(sensor => sensor.loc != loc && sensor.closestBeacon != loc))
      .map { loc => (loc, canHaveBeacon(loc, sensors))
    }
    reachableSensorCount = unreachbleLocations.count((_, isUnreacable) => !isUnreacable)
    //_ <- IO(println(unreachbleLocations))
    _ <- IO(println(s"Positions where beacon cannot be present: $reachableSensorCount"))
  } yield ()
}
