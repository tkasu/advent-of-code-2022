package tkasu.aoc22.challenges.day12

import cats.implicits._
import cats.effect.{IO, IOApp, Ref}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}
import tkasu.aoc22.utils.matrix._
import tkasu.aoc22.utils.string._

object part1 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day12/part1/input_example")
  private val inputResource = makeSourceResource("day12/part1/input")

  enum Direction {
    case Up, Down, Left, Right

    def oppositeTo(other: Direction): Boolean =
      (this, other) match {
        case (Direction.Left, Direction.Right) => true
        case (Direction.Right, Direction.Left) => true
        case (Direction.Up, Direction.Down)    => true
        case (Direction.Down, Direction.Up)    => true
        case _                                 => false
      }

    def oppositeTo(other: Option[Direction]): Boolean =
      other match {
        case None      => false
        case Some(dir) => oppositeTo(dir)
      }
  }

  case class Location(row: Int, column: Int) {

    def move(dir: Direction): Location =
      dir match {
        case Direction.Up    => Location(row - 1, column)
        case Direction.Down  => Location(row + 1, column)
        case Direction.Left  => Location(row, column - 1)
        case Direction.Right => Location(row, column + 1)
      }
  }

  object Location:
    def fromTuple(t: (Int, Int)): Location = Location(t._1, t._2)

  type Steps = List[Direction]

  case class TravelState(
      elfLoc: Location,
      targetLoc: Location,
      curHeight: Int,
      steps: Steps,
      visitLocations: Set[Location]
  ) {

    private def canMoveLeft(heights: HeightMap): Boolean =
      if (elfLoc.column == 0) false
      else if (heights.heightMatrix(elfLoc.row)(elfLoc.column - 1) > curHeight + 1) false
      else true

    private def canMoveRight(heights: HeightMap): Boolean =
      if (elfLoc.column == (heights.width - 1)) false
      else if (heights.heightMatrix(elfLoc.row)(elfLoc.column + 1) > curHeight + 1) false
      else true

    private def canMoveUp(heights: HeightMap): Boolean =
      if (elfLoc.row == 0) false
      else if (heights.heightMatrix(elfLoc.row - 1)(elfLoc.column) > curHeight + 1) false
      else true

    private def canMoveDown(heights: HeightMap): Boolean =
      if (elfLoc.row == (heights.height - 1)) false
      else if (heights.heightMatrix(elfLoc.row + 1)(elfLoc.column) > curHeight + 1) false
      else true

    def possibleDirections(heights: HeightMap): List[Direction] =
      List(
        if (canMoveLeft(heights)) Some(Direction.Left) else None,
        if (canMoveRight(heights)) Some(Direction.Right) else None,
        if (canMoveUp(heights)) Some(Direction.Up) else None,
        if (canMoveDown(heights)) Some(Direction.Down) else None
      ).flatten

    def atTarget: Boolean = elfLoc == targetLoc
  }

  object TravelState:
    private def findStart(charMatrix: Array[Array[Char]]): (Int, Int) =
      findMatrixFirstMatchIndex(charMatrix, 'S').get

    private def findTarget(charMatrix: Array[Array[Char]]): (Int, Int) =
      findMatrixFirstMatchIndex(charMatrix, 'E').get

    def fromRaw(input: String): TravelState =
      val charMap = input
        .split("\n")
        .map(_.toCharArray)
      val start  = Location.fromTuple(findStart(charMap))
      val target = Location.fromTuple(findTarget(charMap))
      TravelState(start, target, charsToHeights('S'), List.empty, Set(start))

  case class HeightMap(heightMatrix: Array[Array[Int]]) {

    lazy val height: Int = heightMatrix.length
    lazy val width: Int  = heightMatrix(0).length

    override def toString = stringifyMatrix(heightMatrix, "HeightMap")
  }

  val charsToHeights: Map[Char, Int] = {
    ('a' to 'z').zip(1 to 26).toMap + ('S' -> 1) + ('E' -> 26)
  }

  object HeightMap:

    def fromRaw(input: String) =
      val charMap = input
        .split("\n")
        .map(_.toCharArray)
      HeightMap(charMap.map(_.map(charsToHeights)))

  def findNextMoves(state: TravelState, heightMap: HeightMap): Option[List[TravelState]] =
    val moveDirs = state
      .possibleDirections(heightMap)
      .filterNot(_.oppositeTo(state.steps.headOption))
    if (moveDirs.isEmpty) None
    else
      val newStates = moveDirs
        .map(dir => (dir, state.elfLoc.move(dir)))
        .filterNot((_, loc) => state.visitLocations.contains(loc))
        .map { (dir, loc) =>
          state.copy(
            elfLoc = loc,
            steps = dir :: state.steps,
            curHeight = heightMap.heightMatrix(loc.row)(loc.column),
            visitLocations = state.visitLocations + loc
          )
        }
      if (newStates.isEmpty) None
      else Some(newStates)

  /** Find routes in parallel with Cats Effect IOs
    *   - Uses resultRef to store the shortest path
    *   - Uses shortestPathToLocRef to optimize that recursion is stopped if the current location
    *     has been visited with fewer steps before by some other possible route
    */
  private def findRoutesParallel(
      state: TravelState,
      heightMap: HeightMap,
      resultRef: Ref[IO, Option[Steps]],
      shortestPathToLocRef: Ref[IO, Map[Location, Int]]
  ): IO[Unit] =

    def updateResultRef(state: TravelState, resultRef: Ref[IO, Option[Steps]]): IO[Unit] =
      resultRef.modify[Unit] {
        case None => (Some(state.steps.reverse), ())
        case curSteps @ Some(steps) =>
          if (steps.length <= state.steps.length) (curSteps, ())
          else (Some(state.steps.reverse), ())
      }

    def maybeUpdateLocMapRef(
        state: TravelState,
        shortestPathToLocRef: Ref[IO, Map[Location, Int]]
    ): IO[Boolean] =
      shortestPathToLocRef.modify[Boolean] { map =>
        val curLen = state.steps.length
        map.get(state.elfLoc) match {
          case None => (map + (state.elfLoc -> curLen), true)
          case Some(len) =>
            if (len <= curLen) (map, false) else (map + (state.elfLoc -> curLen), true)
        }
      }

    for {
      _ <- IO.unit
      foundTarget = state.atTarget
      _ <-
        if (foundTarget)
          IO(println(addThreadPrefix(s"Found route with length: ${state.steps.length}.")))
            >> updateResultRef(state, resultRef)
        else IO.unit
      isCurrentlyShortestToLoc <- maybeUpdateLocMapRef(state, shortestPathToLocRef)
      newStates =
        if (!foundTarget && isCurrentlyShortestToLoc) findNextMoves(state, heightMap) else None
      listOfIos = newStates match {
        case None => List.empty
        case Some(states) =>
          states.map(newState =>
            findRoutesParallel(newState, heightMap, resultRef, shortestPathToLocRef)
          )
      }
      _ <- listOfIos.parSequence
    } yield ()

  def findShortestRoute(state: TravelState, heightMap: HeightMap): IO[Option[Steps]] = for {
    _ <- IO(println(addThreadPrefix(s"Starting route finding for $state")))
    initStepsState: Option[Steps] = None
    resultRef            <- Ref[IO].of(initStepsState)
    shortestPathToLocRef <- Ref[IO].of(Map.empty[Location, Int])
    _                    <- findRoutesParallel(state, heightMap, resultRef, shortestPathToLocRef)
    shortestRoute        <- resultRef.get
  } yield shortestRoute

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    heightMap = HeightMap.fromRaw(input)
    state     = TravelState.fromRaw(input)
  } yield (heightMap, state)

  override def run = for {
    parsed <- parseFile()
    heightMap = parsed._1
    state     = parsed._2
    shortestRoute <- findShortestRoute(state, heightMap)
    shortestRouteSize = shortestRoute.map(_.size)
    _ <- IO(println(shortestRoute))
    _ <- IO(println(s"Shortest route length: ${shortestRouteSize}"))
  } yield ()
}
