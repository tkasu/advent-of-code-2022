package tkasu.aoc22.utils

import cats.effect.{IO, Resource}

import scala.io.Source

object files {

  def readResource(path: String): IO[Source] = IO(Source.fromResource(path))

  def readLines(source: Source): IO[String] = IO(source.getLines().mkString("\n"))

  def closeFile(source: Source): IO[Unit] = IO(source.close())

  def makeSourceResource(srcIO: IO[Source]): Resource[IO, Source] =
    Resource.make(srcIO)(x => closeFile(x))

  def makeSourceResource(path: String): Resource[IO, Source] =
    makeSourceResource(readResource(path))

}
