package tkasu.aoc22.challenges.day13

import cats.effect.{IO, IOApp}
import tkasu.aoc22.challenges.day13.part1.{PacketInt, PacketList, Pair, parseFile}

object part2 extends IOApp.Simple {

  val fstDividerPacket = PacketList(List(PacketList(List(PacketInt(2)))))
  val sndDividerPacket = PacketList(List(PacketList(List(PacketInt(6)))))

  def flattenPairs(pairs: Seq[Pair]): Seq[PacketList] =
    pairs.foldLeft[Seq[PacketList]](List.empty) { case (flattened, nextPair) =>
      flattened :+ nextPair.left :+ nextPair.right
    }

  def sortPackets(pairs: Seq[PacketList]): Seq[PacketList] =
    pairs.sortWith((packet1, packet2) =>
      Pair(packet1, packet2).inRightOrder() match {
        case Some(bool) => bool
        case _          => true // equal, leave as it is
      }
    )

  def findDividerPackages(packages: Seq[PacketList]): (Int, Int) =
    def findPacketIdx(findPacket: PacketList): Int =
      packages.zipWithIndex
        .find((packet, _) => packet == findPacket)
        // Wanted indices start from 1
        .map((_, idx) => idx + 1)
        .get

    (findPacketIdx(fstDividerPacket), findPacketIdx(sndDividerPacket))

  override def run = for {
    parsed <- parseFile()
    // _      <- IO(println(parsed))
    flattenedPackets = flattenPairs(parsed) :+ fstDividerPacket :+ sndDividerPacket
    // _ <- IO(println(flattenedPackets))
    sortedPackets = sortPackets(flattenedPackets)
    // _ <- IO(println(sortedPackets.mkString("\n")))
    dividerPackageIndices = findDividerPackages(sortedPackets)
    _ <- IO(
      println(
        s"Divider packages found from indices: $dividerPackageIndices, multiply of those is ${dividerPackageIndices._1 * dividerPackageIndices._2}"
      )
    )
  } yield ()
}
