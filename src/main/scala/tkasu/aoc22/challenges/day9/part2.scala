package tkasu.aoc22.challenges.day9

import cats.effect.{IO, IOApp}
import tkasu.aoc22.challenges.day9.part1.{Direction, Grid, Move, parseFile, stringifyMatrix}

object part2 extends IOApp.Simple {

  enum DirectionExtended {
    case Left, Right, Up, Down, UpLeft, UpRight, DownRight, DownLeft

    def toDirection: Direction = this match {
      case DirectionExtended.Up    => Direction.Up
      case DirectionExtended.Down  => Direction.Down
      case DirectionExtended.Left  => Direction.Left
      case DirectionExtended.Right => Direction.Right
    }
  }

  object DirectionExtended:
    def fromDirection(dir: Direction): DirectionExtended = dir match {
      case Direction.Up    => DirectionExtended.Up
      case Direction.Down  => DirectionExtended.Down
      case Direction.Left  => DirectionExtended.Left
      case Direction.Right => DirectionExtended.Right
    }

  case class MoveExtended(direction: DirectionExtended, count: Int) {
    def toMove: Move = Move(direction.toDirection, count)
  }

  object MoveExtended:
    def fromMove(move: Move): MoveExtended =
      MoveExtended(DirectionExtended.fromDirection(move.direction), move.count)

    def flattenMoves(move: MoveExtended): Seq[MoveExtended] =
      for _ <- (0 until move.count) yield MoveExtended(move.direction, 1)

  object GridExtensions {
    extension (grid: Grid) {

      def moveLocByOne(loc: (Int, Int), dir: DirectionExtended): (Int, Int) =
        dir match {
          case DirectionExtended.Right     => (loc._1, loc._2 + 1)
          case DirectionExtended.Left      => (loc._1, loc._2 - 1)
          case DirectionExtended.Up        => (loc._1 + 1, loc._2)
          case DirectionExtended.Down      => (loc._1 - 1, loc._2)
          case DirectionExtended.UpRight   => (loc._1 + 1, loc._2 + 1)
          case DirectionExtended.UpLeft    => (loc._1 + 1, loc._2 - 1)
          case DirectionExtended.DownRight => (loc._1 - 1, loc._2 + 1)
          case DirectionExtended.DownLeft  => (loc._1 - 1, loc._2 - 1)
        }

      def getDiagDirExt(tailLoc: (Int, Int), headLoc: (Int, Int)): DirectionExtended =
        val (rowMove, columnMove) = Grid.diagMoveDirs(tailLoc, headLoc)
        (rowMove, columnMove) match {
          case (Direction.Left, Direction.Up)    => DirectionExtended.UpLeft
          case (Direction.Left, Direction.Down)  => DirectionExtended.DownLeft
          case (Direction.Right, Direction.Up)   => DirectionExtended.UpRight
          case (Direction.Right, Direction.Down) => DirectionExtended.DownRight
          case _ => throw IllegalArgumentException(s"Invalid combination: $rowMove, $columnMove")
        }

      def getDiagMoveExt(tailLoc: (Int, Int), headLoc: (Int, Int)): MoveExtended =
        val dir = getDiagDirExt(tailLoc, headLoc)
        MoveExtended(dir, 1)

      def applyMoveAndGetTailMoves(moveExt: MoveExtended): (Grid, Seq[MoveExtended]) =
        val (newHeadLoc, newTailLoc, newTailVisitLocs, tailMoves) =
          (0 until moveExt.count).foldLeft(
            grid.headLoc,
            grid.tailLoc,
            grid.tailVisitLocs,
            Seq.empty[MoveExtended]
          ) { case ((headLocState, tailLocState, tailVisitLocsState, tailMovesState), _) =>
            val newHeadLoc       = grid.moveLocByOne(headLocState, moveExt.direction)
            val sameRowColumnDir = grid.sameRowOrColumnDir(tailLocState, newHeadLoc)
            val (newTailLoc, newTailMoves) =
              if (Grid.nextToEachOther(tailLocState, newHeadLoc)) (tailLocState, tailMovesState)
              else if (sameRowColumnDir.isDefined)
                (
                  grid.moveLocByOne(tailLocState, sameRowColumnDir.get),
                  tailMovesState :+ MoveExtended(sameRowColumnDir.get, 1)
                )
              else {
                val tailMove = getDiagMoveExt(tailLocState, newHeadLoc)
                (grid.moveLocByOne(tailLocState, tailMove.direction), tailMovesState :+ tailMove)
              }
            (newHeadLoc, newTailLoc, tailVisitLocsState + newTailLoc, newTailMoves)
          }
        (
          grid.copy(headLoc = newHeadLoc, tailLoc = newTailLoc, tailVisitLocs = newTailVisitLocs),
          tailMoves
        )

      def sameRowOrColumnDir(tailLoc: (Int, Int), headLoc: (Int, Int)): Option[DirectionExtended] =
        if (tailLoc._1 == headLoc._1) {
          val widthDiff = tailLoc._2 - headLoc._2
          if (widthDiff.abs > 2) None
          else if (widthDiff == 0) None
          else if (widthDiff > 0) Some(DirectionExtended.Left)
          else Some(DirectionExtended.Right)
        } else if (tailLoc._2 == headLoc._2) {
          val heightDiff = tailLoc._1 - headLoc._1
          if (heightDiff.abs > 2) None
          else if (heightDiff == 0) None
          else if (heightDiff > 0) Some(DirectionExtended.Down)
          else Some(DirectionExtended.Up)
        } else None
    }

  }

  import GridExtensions._

  case class GridLongTailed(grids: Seq[Grid]) {

    def tailVisitLocs: Set[(Int, Int)] =
      // There is one extra grid to help with the visualization
      // The last grids head is the tail of the whole chain
      // As last grids head = second lasts tail, we can get the correct
      // values from there
      grids.takeRight(2).head.tailVisitLocs

    def tailVisitLocsAdjusted: Set[(Int, Int)] =
      tailVisitLocs.map { case (height, width) =>
        Grid.adjustNegativeLoc((height, width), heightMinLoc, widthMinLoc)
      }

    def width: Int =
      grids.map(_.width).max

    def height: Int =
      grids.map(_.height).max

    def widthMinLoc: Int  = grids.map(_.widthMinLoc).min
    def heightMinLoc: Int = grids.map(_.heightMinLoc).min

    def locations: Array[Array[Option[Seq[Int]]]] =
      Array.ofDim[Option[Seq[Int]]](height, width).zipWithIndex.map { case (row, rowIdx) =>
        row.indices.map { columnIdx =>
          val gridsInLoc = grids.zipWithIndex.filter { case (grid, _) =>
            Grid.adjustNegativeLoc(grid.headLoc, heightMinLoc, widthMinLoc) == (rowIdx, columnIdx)
          }.toList

          gridsInLoc match {
            case List()                          => None
            case gridsWithIdx: List[(Grid, Int)] => Some(gridsWithIdx.map(_._2))
          }
        }.toArray
      }

    def applyMove(move: MoveExtended): GridLongTailed =
      val (newGridLongTail, _) = grids.zipWithIndex.foldLeft(this, Seq(move)) {
        case ((gridLongTailState, nextMoves), (grid, gridIdx)) =>
          val (updatedGrid, updatedTailMoves) = nextMoves.foldLeft(grid, Seq.empty[MoveExtended]) {
            case ((gridState, tailMoves), move) =>
              val (newGrid, newTailMoves) = gridState.applyMoveAndGetTailMoves(move)
              (newGrid, tailMoves ++ newTailMoves)
          }
          (
            gridLongTailState.copy(grids = gridLongTailState.grids.updated(gridIdx, updatedGrid)),
            updatedTailMoves
          )
      }
      newGridLongTail

    def gridForPrint: Array[Array[String]] =
      locations.reverse.map(_.map {
        case Some(gridIndices) => gridIndices.min.toString
        case None              => "."
      })

    def tailVisitLocGrid: Array[Array[Boolean]] =
      Array.ofDim[Boolean](height, width).zipWithIndex.map { case (row, rowIdx) =>
        row.indices.map { columnIdx =>
          tailVisitLocsAdjusted.contains((rowIdx, columnIdx))
        }.toArray
      }

    def stringifyTailVisitLocations: String =
      stringifyMatrix(
        tailVisitLocGrid.reverse.map(_.map { visited =>
          if (visited) '#' else '.'
        }),
        "TailVisitLocations"
      )

    override def toString: String = stringifyMatrix(gridForPrint, "Locations")
  }

  object GridLongTailed:

    def fromTailLen(tailLen: Int): GridLongTailed =
      GridLongTailed((0 to tailLen).map(_ => Grid((0, 0), (0, 0), Set((0, 0)))))

  def processMoves(
      grid: GridLongTailed,
      moves: Seq[MoveExtended],
      debug: Boolean
  ): IO[GridLongTailed] =
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
    moves <- parseFile().map(_.map(MoveExtended.fromMove))
    // for debugging
    // flattenedMoves        = moves.flatMap(MoveExtended.flattenMoves)
    initialLongTailedGrid = GridLongTailed.fromTailLen(tailLen = 9)
    // _                     <- IO(println(s"Initial long tailed grid: $initialLongTailedGrid"))
    longTailedGridUpdated <- processMoves(initialLongTailedGrid, moves, debug = false)
    visitedPositionsCount = longTailedGridUpdated.tailVisitLocs.size
    _ <- IO(println(s"Visited positions $visitedPositionsCount"))
  } yield ()
}
