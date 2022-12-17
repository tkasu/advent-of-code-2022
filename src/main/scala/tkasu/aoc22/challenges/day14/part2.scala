package tkasu.aoc22.challenges.day14

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.matrix.*
import tkasu.aoc22.challenges.day14.part1.{parseFile, Cave, Vector, Location}

object part2 extends IOApp.Simple {

  object CaveExtensions {
    extension (cave: Cave) {
      def addFloor(): Cave =
        // Limit the width of the floor for better visualizations
        val floorMinWidth = cave.minNonAirWidth - 1000
        val floorMaxWidth = cave.width + 1000
        val floorRow      = cave.height + 1
        val floorStart    = Location(floorRow, floorMinWidth)
        val floorEnd      = Location(floorRow, floorMaxWidth)
        val floorLocs     = Vector(floorStart, floorEnd).toLocations
        cave.copy(rocks = cave.rocks ++ floorLocs.toSet)
    }
  }

  import CaveExtensions._

  override def run = for {
    rockVectors <- parseFile()
    // _           <- IO(println(rockVectors))
    cave = Cave.fromRockVectors(rockVectors).addFloor()
    _ <- IO(println(cave))
    // This is relatively slow, takes ~ 1min to complete
    // There would be a smarter solution in which floor would be added to the logic
    // instead of just adding floor locations to Cave, but I'm feeling lazy atm
    filledCave = cave.fillSand(debug = false)
    _ <- IO(println(filledCave))
    sandOnTopOfTheRocks = filledCave match {
      case None => 0
      // Add sand drop location to amount
      case Some(caveWithSand) => caveWithSand.sand.size + 1
    }
    _ <- IO(println(s"Amount of sand in cave $sandOnTopOfTheRocks"))
  } yield ()
}
