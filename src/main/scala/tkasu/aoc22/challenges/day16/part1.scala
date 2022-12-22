package tkasu.aoc22.challenges.day16

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.parallel._
import tkasu.aoc22.utils.string._
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

object part1 extends IOApp.Simple {

  private val inputResource = makeSourceResource("day16/part1/input_example")
  // private val inputResource = makeSourceResource("day16/part1/input")

  case class TravelState(
      currentValve: Valve,
      openValves: Set[Valve],
      valveMap: Map[String, Valve],
      pressureReleased: Int,
      timeLeft: Int
  ):

    def passTime(): TravelState =
      val newPressureReleased = if (openValves.isEmpty) 0 else openValves.map(_.rate).sum
      this.copy(
        timeLeft = timeLeft - 1,
        pressureReleased = pressureReleased + newPressureReleased
      )

    def openValve(): TravelState =
      this
        .passTime()
        .copy(
          openValves = openValves + currentValve
        )

    def moveTo(id: String): TravelState =
      if (!currentValve.routes.contains(id))
        throw IllegalArgumentException(s"Trying to travel $id from $this")
      this
        .passTime()
        .copy(
          currentValve = valveMap(id)
        )

    def bestTheoreticalFuturePressure(): Int =
      val currentOpenPressures = timeLeft * openValves.map(_.rate).sum
      val remainingValves = valveMap.values.toSet
        .diff(openValves)
        .map(_.rate).toSeq
        .sorted(Ordering.Int.reverse)
      // it takes 1 minute to move to new location, and one minute to open valve
      // so it takes 2 minutes to get new valve flowing (except if current one can be opened)
      // this function is not totally accurate, but should help eliminating bad routes
      val futureOpeningMax = remainingValves.foldLeft((
        currentValve.open,
        timeLeft,
        0
      )) {
        case (acc @ (firstIsOpen, timeLeftState, pressure), next) =>
          if (timeLeftState <= 0) acc
          else
            val timePass = if (firstIsOpen) 1 else 2
            (false, timeLeftState - timePass, pressure + (next * timeLeftState))
      }._3
      pressureReleased + currentOpenPressures + futureOpeningMax

  case class Valve(id: String, rate: Int, open: Boolean, routes: Seq[String])

  object Valve:
    def fromRaw(line: String): Valve =
      def cleanInput(id: String, rateInputStr: String, valveRoutes: Seq[String]): Valve =
        val rate   = rateInputStr.replace("rate=", "").replace(";", "").toInt
        val routes = valveRoutes.map(_.trim.replace(",", ""))
        Valve(id, rate, false, routes)

      line.split(" ").toList match {
        case "Valve" :: id :: "has" :: "flow" :: rateInput :: "tunnels" :: "lead" :: "to" :: "valves" :: valveRoutes =>
          cleanInput(id, rateInput, valveRoutes)
        case "Valve" :: id :: "has" :: "flow" :: rateInput :: "tunnel" :: "leads" :: "to" :: "valve" :: valveRoutes =>
          cleanInput(id, rateInput, valveRoutes)
      }

  def valvesToMap(valves: Seq[Valve]): Map[String, Valve] =
    valves.map(valve => valve.id -> valve).toMap

  def findBestRoute(state: TravelState, resultRef: Ref[IO, Option[TravelState]]): IO[Unit] =
    for {
      _ <- IO.unit
      timeIsOut = state.timeLeft == 0
      _ <-
        if (timeIsOut) resultRef.modify {
          case None => (Some(state), ())
          case Some(curBestState) =>
            if (state.pressureReleased > curBestState.pressureReleased)
              println(
                addThreadPrefix(
                  s"Found new best route with pressure: ${state.pressureReleased}, end state: $state"
                )
              )
              (Some(state), ())
            else (Some(curBestState), ())
        }
        else IO.unit
      nextMoveStates = state.currentValve.routes.map(routeId => state.moveTo(routeId))
      nextMoveAndOpenStates = if (state.currentValve.open) nextMoveStates else nextMoveStates :+ state.openValve()
      nextStateJobs = nextMoveAndOpenStates.map(newState => findBestRoute(newState, resultRef))
      curBestState <- resultRef.get
      _ <- if (timeIsOut) IO.unit
        else if (state.bestTheoreticalFuturePressure() < curBestState.map(_.pressureReleased).getOrElse(0))
          IO.unit
          //IO(println(addThreadPrefix(s"Ending bad route: $state")))
        else nextStateJobs.parSequence
    } yield ()

  def findBestRouteSetup(initialState: TravelState): IO[Option[TravelState]] =
    for {
      _ <- IO.unit
      initialRes: Option[TravelState] = None
      bestRouteRef <- Ref[IO].of(initialRes)
      _            <- findBestRoute(initialState, bestRouteRef)
      result       <- bestRouteRef.get
    } yield result

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    valves = input.split("\n").map(Valve.fromRaw)
  } yield valves.toSeq

  override def run = for {
    valves <- parseFile()
    _      <- IO(println(valves))
    valveMap = valvesToMap(valves)
    initialState = TravelState(
      currentValve = valveMap("AA"),
      openValves = Set.empty,
      valveMap = valveMap,
      pressureReleased = 0,
      timeLeft = 30
    )
    bestRouteState <- findBestRouteSetup(initialState)
    _              <- IO(println(bestRouteState))
  } yield ()
}
