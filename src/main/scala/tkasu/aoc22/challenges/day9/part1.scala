package tkasu.aoc22.challenges.day9

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day9/part1/input")

  enum GridMarkers:
    case Tail, Head, Start

  case class Grid(tailLoc: (Int, Int), headLoc: (Int, Int), tailVisitLocs: Set[(Int, Int)]) {

    def startLoc: (Int, Int) = (0, 0)

    def widthMinLoc: Int  = (tailLoc._2, headLoc._2, tailVisitLocs.map(_._2).min).toList.min
    def heightMinLoc: Int = (tailLoc._1, headLoc._1, tailVisitLocs.map(_._1).min).toList.min

    def width: Int =
      val widthMaxLoc = (tailLoc._2, headLoc._2, tailVisitLocs.map(_._2).max).toList.max
      (widthMaxLoc - widthMinLoc) + 1

    def height: Int =
      val heightMaxLoc = (tailLoc._1, headLoc._1, tailVisitLocs.map(_._1).max).toList.max
      (heightMaxLoc - heightMinLoc) + 1

    // Adjust negative values to positive ones for printing
    def startLocAdjusted: (Int, Int) = Grid.adjustNegativeLoc(startLoc, heightMinLoc, widthMinLoc)
    def tailLocAdjusted: (Int, Int)  = Grid.adjustNegativeLoc(tailLoc, heightMinLoc, widthMinLoc)
    def headLocAdjusted: (Int, Int)  = Grid.adjustNegativeLoc(headLoc, heightMinLoc, widthMinLoc)

    def tailVisitLocsAdjusted: Set[(Int, Int)] =
      tailVisitLocs.map { case (height, width) =>
        Grid.adjustNegativeLoc((height, width), heightMinLoc, widthMinLoc)
      }

    def locations: Array[Array[Option[Seq[GridMarkers]]]] =
      Array.ofDim[Option[Seq[GridMarkers]]](height, width).zipWithIndex.map { case (row, rowIdx) =>
        row.indices.map { columnIdx =>
          val markersInLoc = (
            (GridMarkers.Tail, tailLocAdjusted),
            (GridMarkers.Head, headLocAdjusted),
            (GridMarkers.Start, startLocAdjusted)
          ).toList
            .filter { case (_, loc) =>
              loc == (rowIdx, columnIdx)
            }
            .map { case (marker, _) =>
              marker
            }

          markersInLoc match {
            case List()                     => None
            case markers: List[GridMarkers] => Some(markers)
          }
        }.toArray
      }

    def tailVisitLocGrid: Array[Array[Boolean]] =
      Array.ofDim[Boolean](height, width).zipWithIndex.map { case (row, rowIdx) =>
        row.indices.map { columnIdx =>
          tailVisitLocsAdjusted.contains((rowIdx, columnIdx))
        }.toArray
      }

    def applyMove(move: Move): Grid =
      val (newHeadLoc, newTailLoc, newTailVisitLocs) =
        (0 until move.count).foldLeft(headLoc, tailLoc, tailVisitLocs)((locTuple, _) => {
          val headLocTmp       = locTuple._1
          val tailLocTmp       = locTuple._2
          val tailVisitLocsTmp = locTuple._3
          val newHeadLoc       = Grid.moveLocByOne(headLocTmp, move.direction)
          val newTailLoc =
            if (Grid.nextToEachOther(tailLocTmp, newHeadLoc)) tailLocTmp
            else if (Grid.nearSameRowOrColumn(tailLocTmp, newHeadLoc))
              Grid.moveLocByOne(tailLocTmp, move.direction)
            else Grid.moveTailDiag(tailLocTmp, newHeadLoc)
          (newHeadLoc, newTailLoc, tailVisitLocsTmp + newTailLoc)
        })
      this.copy(headLoc = newHeadLoc, tailLoc = newTailLoc, tailVisitLocs = newTailVisitLocs)

    def gridForPrint: Array[Array[Char]] =
      locations.reverse.map(_.map {
        case Some(markers) =>
          if (markers.contains(GridMarkers.Head)) 'H'
          else if (markers.contains(GridMarkers.Tail)) 'T'
          else 's'
        case None => '.'
      })

    def stringifyTailVisitLocations: String =
      stringifyMatrix(
        tailVisitLocGrid.reverse.map(_.map { visited =>
          if (visited) '#' else '.'
        }),
        "TailVisitLocations"
      )

    override def toString: String = stringifyMatrix(gridForPrint, "Locations")
  }

  object Grid:
    def moveLocByOne(loc: (Int, Int), dir: Direction): (Int, Int) =
      dir match {
        case Direction.Right => (loc._1, loc._2 + 1)
        case Direction.Left  => (loc._1, loc._2 - 1)
        case Direction.Up    => (loc._1 + 1, loc._2)
        case Direction.Down  => (loc._1 - 1, loc._2)
      }

    def nextToEachOther(tailLoc: (Int, Int), headLoc: (Int, Int)): Boolean =
      ((tailLoc._1 - headLoc._1).abs <= 1) && ((tailLoc._2 - headLoc._2).abs <= 1)

    def nearSameRowOrColumn(tailLoc: (Int, Int), headLoc: (Int, Int)): Boolean =
      if ((tailLoc._1 == headLoc._1) && ((tailLoc._2 - headLoc._2).abs <= 2)) true
      else if ((tailLoc._2 == headLoc._2) && ((tailLoc._1 - headLoc._1).abs <= 2)) true
      else false

    def diagMoveDirs(tailLoc: (Int, Int), headLoc: (Int, Int)): (Direction, Direction) =
      val rowMove    = if ((tailLoc._2 - headLoc._2) > 0) Direction.Left else Direction.Right
      val columnMove = if ((tailLoc._1 - headLoc._1) > 0) Direction.Down else Direction.Up
      (rowMove, columnMove)

    def moveTailDiag(tailLoc: (Int, Int), headLoc: (Int, Int)): (Int, Int) =
      val (dir1, dir2) = diagMoveDirs(tailLoc, headLoc)
      moveLocByOne(moveLocByOne(tailLoc, dir1), dir2)

    def adjustNegativeLoc(loc: (Int, Int), heightMin: Int, widthMin: Int): (Int, Int) = (
      loc._1 + (if (heightMin < 0) heightMin.abs else 0),
      loc._2 + (if (widthMin < 0) widthMin.abs else 0)
    )

  enum Direction:
    case Left, Right, Up, Down

  case class Move(direction: Direction, count: Int)

  object Move:
    def fromRaw(line: String): Move =
      val dirStr :: countStr :: Nil = line.split(" ").toList
      val direction = dirStr match {
        case "L" => Direction.Left
        case "R" => Direction.Right
        case "U" => Direction.Up
        case "D" => Direction.Down
        case _   => throw IllegalArgumentException(s"Unknown direction string $dirStr")
      }
      Move(direction = direction, count = countStr.toInt)

  def stringifyMatrix[T](matrix: Array[Array[T]], identifier: String): String =
    matrix.map(_.toSeq.mkString("[", ",", "]")).toSeq.mkString(s"$identifier[\n  ", "\n  ", "\n]")

  def parseFile(): IO[Seq[Move]] = for {
    input <- inputResource.use(src => readLines(src))
    directions = input.split("\n").map(Move.fromRaw)
  } yield directions.toSeq

  def processMoves(grid: Grid, moves: Seq[Move], debug: Boolean): IO[Grid] =
    IO(moves.foldLeft(grid) { case (gridState, move) =>
      val newGridState = gridState.applyMove(move)
      if (debug) {
        println(s"Processing move $move")
        println(newGridState)
        println(newGridState.stringifyTailVisitLocations)
      }
      newGridState
    })

  override def run = for {
    moves <- parseFile()
    initialGrid = Grid(
      (0, 0),
      (0, 0),
      Set((0, 0))
    )
    // _    <- IO(println(s"Moves: $moves"))
    // _    <- IO(println(s"Initial grid: $initialGrid"))
    grid <- processMoves(initialGrid, moves, debug = false)
    visitedPositionsCount = grid.tailVisitLocs.size
    _ <- IO(println(s"Visited positions $visitedPositionsCount"))
  } yield ()
}
