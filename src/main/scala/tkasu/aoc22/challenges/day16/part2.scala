package tkasu.aoc22.challenges.day16

import cats.Order
import cats.effect.std.PQueue
import cats.effect.{IO, IOApp, Ref}
import cats.syntax.parallel.*
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}
import tkasu.aoc22.utils.string.*

import scala.util.Random

object part2 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day16/part1/input_example")
  private val inputResource = makeSourceResource("day16/part1/input")

  implicit val orderForTravelState: Order[TravelState] = Order.fromLessThan((state1, state2) =>
    state1.bestTheoreticalFuturePressure > state2.bestTheoreticalFuturePressure
  )

  case class TravelState(
      currentValve: Valve,
      openValves: Set[Valve],
      valveMap: Map[String, Valve],
      pressureReleased: Int,
      timeLeft: Int
  ):

    lazy val id: String =
      s"${currentValve.id}-${openValves.map(_.id).toSeq.sorted.mkString(",")}-$timeLeft"

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

    // this function is not totally accurate,
    // but should help to be a proxy to eliminating bad routes and
    // sort the priority execution queue
    lazy val bestTheoreticalFuturePressure: Int =
      val currentOpenPressures = timeLeft * openValves.map(_.rate).sum
      val remainingValves = valveMap.values.toSet
        .diff(openValves)
        .map(_.rate)
        .toSeq
        .sorted(Ordering.Int.reverse)
      // it takes 1 minute to move to new location, and one minute to open valve
      // so it takes 2 minutes to get new valve flowing (except if current one can be opened)
      val foldInitState = (currentValve.open, timeLeft, 0)
      val futureOpeningMax = remainingValves
        .foldLeft(foldInitState) { case (acc @ (firstIsOpen, timeLeftState, pressure), next) =>
          if (timeLeftState <= 0) acc
          else
            val timePass = if (firstIsOpen) 1 else 2
            (false, timeLeftState - timePass, pressure + (next * timeLeftState))
        }
        ._3
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

  def findBestRoute(
      state: TravelState,
      resultRef: Ref[IO, Option[TravelState]],
      taskQueue: PQueue[IO, TravelState]
  ): IO[Unit] =
    for {
      _ <- IO.unit
      timeIsOut = state.timeLeft == 0
      newBestState <-
        if (timeIsOut) resultRef.modify {
          case None => (Some(state), true)
          case Some(curBestState) =>
            if (state.pressureReleased > curBestState.pressureReleased) (Some(state), true)
            else (Some(curBestState), false)
        }
        else IO.pure(false)
      _ <-
        if (newBestState)
          IO(
            println(
              addThreadPrefix(
                s"Found new best route with pressure: ${state.pressureReleased}, end state: $state"
              )
            )
          )
        else IO.unit
      nextMoveStates = state.currentValve.routes.map(routeId => state.moveTo(routeId))
      nextMoveAndOpenStates =
        if (state.currentValve.open) nextMoveStates else nextMoveStates :+ state.openValve()
      curBestState <- resultRef.get
      _ <-
        if (timeIsOut) IO.unit
        else if (
          state.bestTheoreticalFuturePressure < curBestState.map(_.pressureReleased).getOrElse(0)
        ) IO.unit
        else
          // Execute random job right away, put rest to the queue
          val nextStatesRandomized = Random.shuffle(nextMoveAndOpenStates)
          taskQueue.tryOfferN(nextStatesRandomized.tail.toList)
            >> findBestRoute(
              nextStatesRandomized.head,
              resultRef,
              taskQueue
            )
    } yield ()

  def findBestRouteSetup(initialState: TravelState, parellelism: Int = 8): IO[Option[TravelState]] =

    def processQueue(
        queue: PQueue[IO, TravelState],
        resultRef: Ref[IO, Option[TravelState]],
        bestRouteRef: Ref[IO, Map[String, Int]],
        iterations: Int
    ): IO[Unit] =
      for {
        _      <- IO.unit
        states <- queue.tryTakeN(Some(parellelism))
        // Check what states should we traverse into
        // if some other route has already went through
        // the following combination curValveId - openedValveIds - timeLeft
        // but with more generated pressure, we should just stop.
        // Otherwise, we should update the mapping and continue
        stateUpdateMask <- bestRouteRef.modify { routeMap =>
          val updateMask = states.map { state =>
            val key = state.id
            routeMap.get(key) match {
              case None              => true
              case Some(curPressure) => state.pressureReleased > curPressure
            }
          }
          val newMappings = states
            .zip(updateMask)
            .filter((_, update) => update)
            .map((state, _) => state.id -> state.pressureReleased)
          (routeMap ++ newMappings, updateMask)
        }
        statesToTraverse = states
          .zip(stateUpdateMask)
          .filter((_, update) => update)
          .map((state, _) => state)
        _ <- statesToTraverse.map(state => findBestRoute(state, resultRef, queue)).parSequence
        queueSize <- queue.size
        _ <-
          if (iterations % 100_000 == 0)
            IO(println(addThreadPrefix(s"Queue size $queueSize after $iterations iterations")))
          else IO.unit
        _ <-
          if (queueSize == 0) IO.unit
          else processQueue(queue, resultRef, bestRouteRef, iterations + 1)
      } yield ()

    for {
      _ <- IO.unit
      initialRes: Option[TravelState] = None
      bestRouteRef <- Ref[IO].of(initialRes)
      initalMapRef: Map[String, Int] = Map.empty
      bestRouteByTimeRef <- Ref[IO].of(initalMapRef)
      taskQueue          <- PQueue.unbounded[IO, TravelState]
      _                  <- taskQueue.offer(initialState)
      _                  <- processQueue(taskQueue, bestRouteRef, bestRouteByTimeRef, 0)
      result             <- bestRouteRef.get
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
    bestRouteState <- findBestRouteSetup(
      initialState,
      parellelism = Runtime.getRuntime.availableProcessors() * 4
    )
    _ <- IO(println(bestRouteState))
  } yield ()
}
