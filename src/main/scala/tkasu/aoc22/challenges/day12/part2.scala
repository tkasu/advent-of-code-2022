package tkasu.aoc22.challenges.day12

import cats.effect.{IO, IOApp}
import cats.implicits.*
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}
import tkasu.aoc22.challenges.day12.part1.{
  HeightMap,
  Location,
  charsToHeights,
  findShortestRoute,
  parseFile
}

object part2 extends IOApp.Simple {

  def findLowestElevationStartingPoints(heightMap: HeightMap): List[Location] =
    heightMap.heightMatrix.zipWithIndex.foldLeft(List.empty[Location]) {
      case (matches, (row, rowIdx)) =>
        row.zipWithIndex
          .filter((height, _) => height == 1)
          .map((_, columnIdx) => Location(rowIdx, columnIdx))
          .toList
          ::: matches
    }

  override def run = for {
    parsed <- parseFile()
    heightMap   = parsed._1
    normalState = parsed._2 // Part1 like state that starts from S
    states = findLowestElevationStartingPoints(heightMap)
      .map(loc => normalState.copy(elfLoc = loc))
    shortestRoutes <- states.map(state => findShortestRoute(state, heightMap)).parSequence
    shortestRoute     = shortestRoutes.flatten.minBy(_.size)
    shortestRouteSize = shortestRoute.size
    _ <- IO(println(shortestRoute))
    _ <- IO(println(s"Shortest route length: ${shortestRouteSize}"))
  } yield ()
}
