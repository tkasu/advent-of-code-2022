package tkasu.aoc22.challenges.day8

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}
import tkasu.aoc22.challenges.day8.part1.{parseFile, stringifyMatrix, TreeGrid}

object part2 extends IOApp.Simple {

  object TreeGridExtensions {
    extension (grid: TreeGrid) {

      def howManyTreesVisibleScoreGrid: Array[Array[Int]] =
        grid.heights.zipWithIndex.map { case (row, rowIdx) =>
          row.indices.map(columnIdx => visibleTreesScenicScore(rowIdx, columnIdx)).toArray
        }

      def visibleTreesScenicScore(row: Int, column: Int): Int =
        val rowHeights    = grid.getRow(row)
        val columnHeights = grid.getColumn(column)
        val visibleTrees = List(
          numberOfVisibleLeft(column, rowHeights),
          numberOfVisibleRight(column, rowHeights),
          numberOfVisibleUp(row, columnHeights),
          numberOfVisibleDown(row, columnHeights)
        )
        visibleTrees.product

      private def numberOfTreesVisible(height: Int, treeLineHeights: Array[Int]) =
        val canSeeOver = treeLineHeights.takeWhile(_ < height).length
        if (canSeeOver < treeLineHeights.length) canSeeOver + 1 else canSeeOver

      private def numberOfVisibleLeft(column: Int, rowHeights: Array[Int]): Int =
        val (left, right) = rowHeights.splitAt(column)
        val height        = right.head
        val treeLine      = left.reverse
        numberOfTreesVisible(height, treeLine)

      private def numberOfVisibleRight(column: Int, rowHeights: Array[Int]): Int =
        val (_, right) = rowHeights.splitAt(column)
        val height     = right.head
        val treeLine   = right.tail
        numberOfTreesVisible(height, treeLine)

      private def numberOfVisibleUp(row: Int, columnHeights: Array[Int]): Int =
        numberOfVisibleLeft(row, columnHeights)

      private def numberOfVisibleDown(row: Int, columnHeights: Array[Int]): Int =
        numberOfVisibleRight(row, columnHeights)
    }
  }

  import TreeGridExtensions._

  override def run = for {
    grid <- parseFile()
    // _ <- IO(println(grid))
    visibilityGrid = grid.howManyTreesVisibleScoreGrid
    // _ <- IO(println(stringifyMatrix(visibilityGrid, "HowManyTreesVisibleScoreGrid")))
    maxScore = visibilityGrid.flatten.max
    _ <- IO(println(s"Max visibility scenic score: $maxScore"))
  } yield ()
}
