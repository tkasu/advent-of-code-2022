package tkasu.aoc22.challenges.day7

import cats.effect.{IO, IOApp}
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

import scala.annotation.tailrec
import scala.util.Try

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day7/part1/input")

  sealed trait FileSystemItem
  case class File(name: String, size: BigInt) extends FileSystemItem
  case class Directory(
      name: String,
      parent: Option[Directory],
      contents: Option[Seq[FileSystemItem]]
  ) extends FileSystemItem {

    def mapFile[A](f: File => A): Seq[A] =
      contents match {
        case None => Seq.empty
        case Some(stuff) =>
          stuff.flatMap {
            case file: File     => Seq(f(file))
            case dir: Directory => dir.mapFile(f)
          }
      }

    def fullPath(): String =
      @tailrec
      def helper(dir: Directory, parts: List[String]): List[String] =
        dir.parent match {
          case None         => dir.name :: parts
          case Some(parent) => helper(parent, dir.name :: parts)
        }
      helper(this, List.empty).mkString("/")
  }

  sealed trait FileSystemCommand
  case class Cd(targetDir: String)         extends FileSystemCommand
  case class Ls(output: Option[Directory]) extends FileSystemCommand

  type ShellLine = Either[FileSystemCommand, FileSystemItem]

  def parseLine(line: String): ShellLine =
    line.split(" ").toList match {
      case "$" :: rest =>
        rest match {
          case "ls" :: _             => Left(Ls(None))
          case "cd" :: target :: Nil => Left(Cd(target))
        }
      case "dir" :: name :: Nil                          => Right(Directory(name, None, None))
      case fst :: snd :: Nil if Try(fst.toInt).isSuccess => Right(File(snd, BigInt(fst)))
      case unknown => throw IllegalArgumentException(s"Can't parse line $unknown")
    }

  type DirContentsMap = Map[String, Seq[FileSystemItem]]

  def outputsToCommands(lines: List[ShellLine]): Seq[FileSystemCommand] =
    def updateContents(
        output: Seq[FileSystemCommand],
        dirContents: DirContentsMap
    ): Seq[FileSystemCommand] =
      // Patch child directories with correct contents
      def patchSubdirContents(m: DirContentsMap): DirContentsMap =
        def patchDir(dir: Directory): Directory =
          val patchedDir = dir.copy(contents = Some(dirContents(dir.fullPath())))
          patchedDir.contents match {
            case None => patchedDir
            case Some(stuff) =>
              patchedDir.copy(contents = Some(stuff.map {
                case file: File => file
                case subDir: Directory =>
                  patchDir(subDir.copy(contents = Some(dirContents(subDir.fullPath()))))
              }))
          }

        m.map { case (k, v) =>
          k -> v.map {
            case dir @ Directory(_, _, _) => patchDir(dir)
            case file @ File(_, _)        => file
          }
        }
      val dirContentsPatched = patchSubdirContents(dirContents)

      output.map {
        case cd @ Cd(_) => cd
        case Ls(someDir) =>
          someDir match {
            case Some(dir) =>
              Ls(Some(dir.copy(contents = Some(dirContentsPatched(dir.fullPath())))))
          }
      }

    @tailrec
    def helper(
        lines: List[ShellLine],
        dirContext: Option[Directory],
        dirContents: DirContentsMap,
        output: Seq[FileSystemCommand]
    ): Seq[FileSystemCommand] =
      lines match {
        case List() => updateContents(output, dirContents)
        case fst :: Nil =>
          throw IllegalArgumentException(s"Only one command left, don't know what to do: $fst")
        case fst :: rest =>
          fst match {
            case Right(_) => throw IllegalArgumentException(s"Expected FileSystemCommand, got $fst")
            case Left(cd @ Cd("..")) =>
              helper(rest, Some(dirContext.get.parent.get), dirContents, output :+ cd)
            case Left(cd @ Cd(targetDirName)) =>
              val dir = Directory(targetDirName, dirContext, None)
              helper(rest, Some(dir), dirContents, output :+ cd)
            case Left(Ls(_)) =>
              val (shellOutputs, shellRest) = rest.span(_.isRight)
              val contents = shellOutputs.map {
                case Right(dir: Directory) => dir.copy(parent = dirContext)
                case Right(other)          => other
              }
              val newDirCache = dirContents + (dirContext.get.fullPath() -> contents)
              helper(shellRest, dirContext, newDirCache, output :+ Ls(dirContext))
          }
      }
    helper(lines, None, Map.empty, Seq.empty)

  def parseFile(): IO[Seq[FileSystemCommand]] = for {
    input <- inputResource.use(src => readLines(src))
    cmdLines = input.split("\n").map(parseLine)
    fs       = outputsToCommands(cmdLines.toList)
  } yield fs

  def dirSizes(cmds: Seq[FileSystemCommand]): Seq[(String, BigInt, Int)] =
    cmds
      .flatMap {
        case Ls(dir) => Some(dir)
        case _       => None
      }
      .map { someDir =>
        val dir = someDir.get
        (dir.name, dir.mapFile(_.size).sum, dir.mapFile(_.size).size)
      }

  override def run = {
    parseFile()
      .map(dirSizes)
      .map(_.filter { (_, size, _) =>
        size <= 100_000
      })
      .map(_.map((_, size, _) => size).sum)
      .map(println)
  }
}
