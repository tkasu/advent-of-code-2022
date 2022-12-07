package tkasu.aoc22.challenges.day7

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}
import tkasu.aoc22.challenges.day7.part1.{dirSizes, parseFile}

val FILESYSTEM_TOTAL_SIZE    = 70_000_000
val NEEDED_SPACE_FOR_CLEANUP = 30_000_000

def getRootDirSpace(dirs: Seq[(String, BigInt, Int)]): BigInt =
  val (name, size, _) :: _ = dirs
  if (name != "/") throw IllegalArgumentException(s"Expected root dir '/' to be the first one")
  size

def findSmallestDirOverSize(
    dirs: Seq[(String, BigInt, Int)],
    smallestSize: BigInt
): Option[(String, BigInt, Int)] =
  dirs
    .sortBy((_, size, _) => size)
    .find((_, size, _) => size >= smallestSize)

object part2 extends IOApp.Simple {
  override def run = for {
    file <- parseFile()
    sizes       = dirSizes(file)
    rootDirSize = getRootDirSpace(sizes)
    _ <- IO(println(s"Root dir size: $rootDirSize"))
    currentFreeSpace = FILESYSTEM_TOTAL_SIZE - rootDirSize
    _ <- IO(println(s"Current free space: $currentFreeSpace"))
    neededExtraSpace = NEEDED_SPACE_FOR_CLEANUP - currentFreeSpace
    _ <- IO(println(s"Needed extra space: $neededExtraSpace"))
    smallestDir = findSmallestDirOverSize(sizes, neededExtraSpace)
    _ <- IO(println(s"Smallest dir to remove: $smallestDir"))
  } yield ()
}
