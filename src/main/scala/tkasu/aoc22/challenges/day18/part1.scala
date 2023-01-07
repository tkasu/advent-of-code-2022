package tkasu.aoc22.challenges.day18

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

object part1 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day18/part1/input_example")
  private val inputResource = makeSourceResource("day18/part1/input")

  enum Side:
    case Left, Right, Up, Down, Front, Back

  case class Location3D(x: Byte, y: Byte, z: Byte):

    def sidesNotCovered(others: Seq[Location3D]): Set[Side] =
      val leftCovered = others.exists(other =>
        other.y == y
          && other.z == z
          && other.x == x - 1
      )
      val rightCovered = others.exists(other =>
        other.y == y
          && other.z == z
          && other.x == x + 1
      )
      val UpCovered = others.exists(other =>
        other.y == y - 1
          && other.z == z
          && other.x == x
      )
      val DownCovered = others.exists(other =>
        other.y == y + 1
          && other.z == z
          && other.x == x
      )
      val FrontCovered = others.exists(other =>
        other.y == y
          && other.z == z + 1
          && other.x == x
      )
      val BackCovered = others.exists(other =>
        other.y == y
          && other.z == z - 1
          && other.x == x
      )

      Set(
        if (leftCovered) None else Some(Side.Left),
        if (rightCovered) None else Some(Side.Right),
        if (UpCovered) None else Some(Side.Up),
        if (DownCovered) None else Some(Side.Down),
        if (FrontCovered) None else Some(Side.Front),
        if (BackCovered) None else Some(Side.Back)
      ).flatten

  object Location3D:
    def fromRaw(line: String): Location3D =
      val xStr :: yStr :: zStr :: Nil = line.split(",").toList
      Location3D(xStr.toByte, yStr.toByte, zStr.toByte)

  case class LavaGrid(locations: Seq[Location3D]):

    def countNonCovered(): Int =
      locations.map(_.sidesNotCovered(locations).size).sum

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    locations = input.split("\n").map(Location3D.fromRaw)
  } yield locations.toSeq

  override def run = for {
    locations <- parseFile()
    grid = LavaGrid(locations)
    // _         <- IO(println(grid))
    _ <- IO(println(grid.countNonCovered()))
  } yield ()
}
