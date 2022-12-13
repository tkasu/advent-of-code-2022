package tkasu.aoc22.utils

import scala.util.control.NonLocalReturns.*

object matrix {

  def stringifyMatrix[T](matrix: Array[Array[T]], identifier: String): String =
    matrix.map(_.toSeq.mkString("[", ",", "]")).toSeq.mkString(s"$identifier[\n  ", "\n  ", "\n]")

  def stringifyMatrixNoSep[T](matrix: Array[Array[T]], identifier: String): String =
    matrix.map(_.toSeq.mkString("[", "", "]")).toSeq.mkString(s"$identifier[\n  ", "\n  ", "\n]")

  def findMatrixFirstMatchIndex[T](matrix: Array[Array[T]], findItem: T): Option[(Int, Int)] =
    returning {
      for (row, rowIdx) <- matrix.zipWithIndex do
        for (item, columnIdx) <- row.zipWithIndex do
          if (item == findItem) throwReturn(Some(rowIdx, columnIdx))
      None
    }

  def findMatrixAllMatchingIndices[T](matrix: Array[Array[T]], findItem: T): List[(Int, Int)] =
    matrix.zipWithIndex.foldLeft(List.empty[(Int, Int)]) {
      case (matches, (row, rowIdx)) =>
        row.zipWithIndex
          .filter((height, _) => height == findItem)
          .map((_, columnIdx) => (rowIdx, columnIdx))
          .toList
          ::: matches
    }

}
