package tkasu.aoc22.challenges.day8

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day8/part1/input")

  case class TreeGrid(heights: Array[Array[Int]]) {

    def getRow(row: Int): Array[Int] =
      heights(row)

    def getColumn(column: Int): Array[Int] =
      heights.map(row => row(column))

    def visibilityGrid: Array[Array[Boolean]] =
      heights.zipWithIndex.map { case (row, rowIdx) =>
        row.indices.map(columnIdx => isVisible(rowIdx, columnIdx)).toArray
      }

    def isVisible(row: Int, column: Int): Boolean =
      val rowHeights    = getRow(row)
      val columnHeights = getColumn(column)
      visibleLeft(column, rowHeights)
      || visibleRight(column, rowHeights)
      || visibleUp(row, columnHeights)
      || visibleDown(row, columnHeights)

    private def isVisibleInLine(height: Int, treeLineHeights: Array[Int]): Boolean =
      !treeLineHeights.exists(_ >= height)

    private def visibleLeft(column: Int, rowHeights: Array[Int]): Boolean =
      val (treeLine, right) = rowHeights.splitAt(column)
      val height            = right.head
      isVisibleInLine(height, treeLine)

    private def visibleRight(column: Int, rowHeights: Array[Int]) =
      val (_, right) = rowHeights.splitAt(column)
      val height     = right.head
      val treeLine   = right.tail
      isVisibleInLine(height, treeLine)

    private def visibleUp(row: Int, columnHeights: Array[Int]) =
      visibleLeft(row, columnHeights)

    private def visibleDown(row: Int, columnHeights: Array[Int]) =
      visibleRight(row, columnHeights)

    override def toString: String =
      stringifyMatrix(heights, "TreeGrid")
  }

  def stringifyMatrix[T](matrix: Array[Array[T]], identifier: String): String =
    matrix.map(_.toSeq.mkString("[", ",", "]")).toSeq.mkString(s"$identifier[\n  ", "\n  ", "\n]")

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    grid = TreeGrid(input.split("\n").map(_.toCharArray.map(_.asDigit)))
  } yield grid

  override def run = for {
    grid <- parseFile()
    // _ <- IO(println(grid))
    visibilityGrid = grid.visibilityGrid
    // _ <- IO(println(stringifyMatrix(visibilityGrid, "VisibilityGrid")))
    visibleTreeCount = visibilityGrid.flatMap(_.filter(isVisible => isVisible)).length
    _ <- IO(println(s"Number of trees visible: $visibleTreeCount"))
  } yield ()
}
