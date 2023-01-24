package tkasu.aoc22.challenges.day22

import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}
import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.matrix._
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

object part1 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day22/part1/input_example")
  private val inputResource = makeSourceResource("day22/part1/input")

  val PLAYER_START_DIR = Direction.Right

  case class State(player: Player, map: Map, remPath: Path):

    def applyNextAction() =
      val nextAction = remPath.actions.head
      val newPlayer = nextAction match {
        case Action.Rotate(rotateDir) => player.copy(dir = player.dir.rotate(rotateDir))
        case Action.Move(amount)      => player.move(map, amount)
      }
      this.copy(newPlayer, map, remPath.copy(actions = remPath.actions.tail))

    @tailrec
    final def applyAllActions(debug: Boolean = false, idx: Int = 0): State =
      if (debug)
        println(s"Map: ${map.debugMatrix(this.player)}")
        println(s"Next moves: ${this.remPath}")
      remPath.actions match {
        case List() => this
        case _      => this.applyNextAction().applyAllActions(debug, idx + 1)
      }

  case class Player(row: Int, column: Int, dir: Direction):
    private def getTilesInMoveDir(map: Map): Array[(Tile, Int)] =
      dir match {
        case Direction.Right => map.getRow(row).zipWithIndex
        case Direction.Left  => map.getRow(row).zipWithIndex.reverse
        case Direction.Down  => map.getColumn(column).zipWithIndex
        case Direction.Up    => map.getColumn(column).zipWithIndex.reverse
      }

    private def prevIdx(idx: Int): Int =
      dir match {
        case Direction.Right => idx - 1
        case Direction.Left  => idx + 1
        case Direction.Down  => idx - 1
        case Direction.Up    => idx + 1
      }

    private def getDirRelativePlayerIdx(map: Map): Int =
      dir match {
        case Direction.Right => column
        case Direction.Left  => map.width - column - 1
        case Direction.Down  => row
        case Direction.Up    => map.height - row - 1
      }

    private def newPlayerFromIdx(idx: Int): Player =
      dir match {
        case Direction.Right => this.copy(column = idx)
        case Direction.Left  => this.copy(column = idx)
        case Direction.Down  => this.copy(row = idx)
        case Direction.Up    => this.copy(row = idx)
      }

    @tailrec
    final def move(map: Map, amount: Int): Player =
      val playerIdx      = getDirRelativePlayerIdx(map)
      val tilesInMoveDir = getTilesInMoveDir(map)
      val tilesInFront   = tilesInMoveDir.slice(playerIdx + 1, tilesInMoveDir.length)

      val blockedTiles = tilesInFront.zipWithIndex.dropWhile { case ((tile, _), dropIdx) =>
        tile == Tile.Open && (dropIdx < amount)
      }
      val moves = blockedTiles.headOption
        .map(_._2)
        .getOrElse(tilesInMoveDir.length - playerIdx - 1)

      blockedTiles.headOption.map(_._1) match {
        case _ if amount == 0                => this
        case Some(_, idx) if moves == amount => newPlayerFromIdx(prevIdx(idx))
        case Some(Tile.Open, idx)            => newPlayerFromIdx(prevIdx(idx))
        case Some(Tile.Wall, idx)            => newPlayerFromIdx(prevIdx(idx))
        case None =>
          val tilesFromOtherSide = tilesInMoveDir
            .dropWhile((tile, _) => tile == Tile.Oblivion)
          if (tilesFromOtherSide.head._1 == Tile.Wall)
            tilesInFront.lastOption.map(_._2) match {
              case Some(idx) => newPlayerFromIdx(idx)
              case None      => this // first/last tile in column/row
            }
          else
            newPlayerFromIdx(tilesFromOtherSide.head._2)
              .move(map, amount - moves - 1)
        case Some(Tile.Oblivion, idx) =>
          val tilesFromOtherSide = tilesInMoveDir
            .dropWhile((tile, _) => tile == Tile.Oblivion)
          if (tilesFromOtherSide.head._1 == Tile.Wall) newPlayerFromIdx(prevIdx(idx))
          else
            newPlayerFromIdx(tilesFromOtherSide.head._2)
              .move(map, amount - moves - 1)
      }

    def password: Int =
      val facingScore = dir match {
        case Direction.Right => 0
        case Direction.Left  => 2
        case Direction.Down  => 1
        case Direction.Up    => 3
      }
      1_000 * (row + 1) + 4 * (column + 1) + facingScore

  object Player:
    def fromMap(map: Map): Player =
      findMatrixFirstMatchIndex(map.tiles, Tile.Open) match {
        case Some((rowIdx, colIdx)) => Player(rowIdx, colIdx, PLAYER_START_DIR)
        case None => throw IllegalArgumentException(s"Could not find open tile from $map")
      }

  enum Direction:
    case Left, Right, Up, Down

    def rotate(rotateDir: RotateDir): Direction =
      (this, rotateDir) match {
        case (Direction.Left, RotateDir.Clockwise)         => Direction.Up
        case (Direction.Up, RotateDir.Clockwise)           => Direction.Right
        case (Direction.Right, RotateDir.Clockwise)        => Direction.Down
        case (Direction.Down, RotateDir.Clockwise)         => Direction.Left
        case (Direction.Left, RotateDir.CounterClockwise)  => Direction.Down
        case (Direction.Up, RotateDir.CounterClockwise)    => Direction.Left
        case (Direction.Right, RotateDir.CounterClockwise) => Direction.Up
        case (Direction.Down, RotateDir.CounterClockwise)  => Direction.Right
      }

  enum Tile:
    case Oblivion, Open, Wall

    def toChar: Char = this match {
      case Tile.Oblivion => ' '
      case Tile.Open     => '.'
      case Tile.Wall     => '#'
    }

  case class Map(tiles: Array[Array[Tile]]):

    lazy val width  = tiles(0).length
    lazy val height = tiles.length

    def getRow(idx: Int): Array[Tile]    = tiles(idx)
    def getColumn(idx: Int): Array[Tile] = tiles.map(row => row(idx))

    def debugMatrix(player: Player): String =
      val charMatrix = tiles.zipWithIndex.map { case (row, rowIdx) =>
        row.zipWithIndex.map { case (tile, colIdx) =>
          if (player.row == rowIdx && player.column == colIdx)
            player.dir match {
              case Direction.Right => '>'
              case Direction.Up    => '^'
              case Direction.Down  => 'v'
              case Direction.Left  => '<'
            }
          else tile.toChar
        }
      }
      stringifyMatrixNoSep(charMatrix, "Map")

    override def toString = stringifyMatrix(tiles, "Map")

  object Map:
    def fromRaw(s: String): Map =
      val chars  = s.split("\n").map(_.toCharArray)
      val height = chars.length
      val width  = chars.map(_.length).max
      val matrix = Array.ofDim[Tile](height, width).zipWithIndex.map { case (row, rowIdx) =>
        row.indices.map { colIdx =>
          Try(chars(rowIdx)(colIdx)) match {
            case Failure(_)   => Tile.Oblivion
            case Success(' ') => Tile.Oblivion
            case Success('.') => Tile.Open
            case Success('#') => Tile.Wall
            case Success(somethingElse) =>
              throw IllegalArgumentException(s"Could not parse $somethingElse")
          }
        }.toArray
      }
      Map(matrix)

  enum RotateDir:
    case Clockwise, CounterClockwise

  enum Action:
    case Rotate(dir: RotateDir)
    case Move(amount: Int)

  case class Path(actions: List[Action]):

    override def toString = actions.mkString("Path[", ",", "]")

  object Path:
    @tailrec
    private def parseActions(line: String, acc: List[Action] = List.empty): List[Action] =
      if (line.isEmpty) acc.reverse
      else
        val (digits, rest) = line.span(_.isDigit)
        val (action, nextLine) = if (digits.isEmpty) rest.head match {
          case 'R' => (Action.Rotate(RotateDir.Clockwise), rest.tail)
          case 'L' => (Action.Rotate(RotateDir.CounterClockwise), rest.tail)
        }
        else (Action.Move(digits.toInt), rest)
        parseActions(nextLine, action :: acc)

    def fromRaw(line: String): Path =
      val actions = parseActions(line)
      Path(actions)

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    mapInput :: pathInput :: Nil = input.split("\n\n").toList
    map                          = Map.fromRaw(mapInput)
    path                         = Path.fromRaw(pathInput)
  } yield (map, path)

  override def run = for {
    inputs <- parseFile()
    map  = inputs._1
    path = inputs._2
    initialState = State(
      Player.fromMap(map),
      map,
      path
    )
    // _ <- IO(println(initialState))
    finalState = initialState.applyAllActions(debug = false)
    _ <- IO(println(finalState.map.debugMatrix(finalState.player)))
    _ <- IO(println(s"Password: ${finalState.player.password}"))
  } yield ()
}
