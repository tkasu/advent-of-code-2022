package tkasu.aoc22.challenges.day16

import cats.Order
import cats.effect.std.PQueue
import cats.effect.{IO, IOApp, Ref}
import cats.syntax.parallel.*
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}
import tkasu.aoc22.utils.string.*

import scala.util.Random
import scala.util.Try

object part2 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day16/part1/input_example")
  private val inputResource = makeSourceResource("day16/part1/input")

  sealed trait Action
  case class Move(to: String) extends Action
  case class OpenValve()      extends Action

  type ValveMap = Map[String, Valve]

  case class TravelState(
      currentValveId: String,
      elephantValveId: String,
      openValveIds: Set[String],
      pressureReleased: Int,
      timeLeft: Int
  ):

    lazy val id: String =
      s"${List(currentValveId, elephantValveId).sorted
          .mkString(",")}-${openValveIds.toSeq.sorted.mkString(",")}-$timeLeft"

    def passTime(valveMap: ValveMap): TravelState =
      val newPressureReleased =
        if (openValveIds.isEmpty) 0 else openValveIds.map(valveMap(_).rate).sum
      this.copy(
        timeLeft = timeLeft - 1,
        pressureReleased = pressureReleased + newPressureReleased
      )

    def doActions(action: Action, elephAction: Action, valveMap: ValveMap): TravelState =
      def checkMove(valve: Valve, targetId: String): Unit =
        if (!valve.routes.contains(targetId))
          throw IllegalArgumentException(s"Trying to travel $targetId from $valve")

      val (newCurrentValveId, newElephValveId, newOpenValves) = (action, elephAction) match {
        case (OpenValve(), OpenValve()) =>
          (currentValveId, elephantValveId, List(currentValveId, elephantValveId))
        case (OpenValve(), Move(toId)) =>
          checkMove(valveMap(elephantValveId), toId)
          (currentValveId, toId, List(currentValveId))
        case (Move(toId), OpenValve()) =>
          checkMove(valveMap(currentValveId), toId)
          (toId, elephantValveId, List(elephantValveId))
        case (Move(toId), Move(elephToId)) =>
          checkMove(valveMap(currentValveId), toId)
          checkMove(valveMap(elephantValveId), elephToId)
          (toId, elephToId, List())
      }
      this
        .passTime(valveMap)
        .copy(
          currentValveId = newCurrentValveId,
          elephantValveId = newElephValveId,
          openValveIds = openValveIds ++ newOpenValves
        )

    // This is the same function from part1
    // But it seems to work better in priority queue than
    // bestTheoreticalFuturePressureForElim, so lets still have it
    private var bestTheoreticalFuturePressureForSortingCache: Option[Int] = None
    def bestTheoreticalFuturePressureForSorting(valveMap: ValveMap): Int =
      if (bestTheoreticalFuturePressureForSortingCache.isDefined)
        bestTheoreticalFuturePressureForSortingCache.get
      else
        val currentOpenPressures = timeLeft * openValveIds.map(valveMap(_).rate).sum
        val remainingValves = valveMap
          .filterNot((key, _) => openValveIds.contains(key))
          .values
          .map(_.rate)
          .toSeq
          .sorted(Ordering.Int.reverse)
        // it takes 1 minute to move to new location, and one minute to open valve
        // so it takes 2 minutes to get new valve flowing (except if current one can be opened)
        val foldInitState = (openValveIds.contains(currentValveId), timeLeft, 0)
        val futureOpeningMax = remainingValves
          .foldLeft(foldInitState) { case (acc @ (firstIsOpen, timeLeftState, pressure), next) =>
            if (timeLeftState <= 0) acc
            else
              val timePass = if (firstIsOpen) 1 else 2
              (false, timeLeftState - timePass, pressure + (next * timeLeftState))
          }
          ._3
        val result = pressureReleased + currentOpenPressures + futureOpeningMax
        bestTheoreticalFuturePressureForSortingCache = Some(result)
        result

    // this function is not totally accurate
    // it is really overly optimistic about possible future value
    // but should help to be a proxy to eliminating bad routes
    var bestTheoreticalFuturePressureForElimCache: Option[Int] = None
    def bestTheoreticalFuturePressureForElim(valveMap: ValveMap): Int =
      if (bestTheoreticalFuturePressureForElimCache.isDefined)
        bestTheoreticalFuturePressureForElimCache.get
      else
        val currentOpenPressures = timeLeft * openValveIds.map(valveMap(_).rate).sum
        val remainingValves = valveMap
          .filterNot((key, _) => openValveIds.contains(key))
          .values
          .map(_.rate)
          .toList
          .sorted(Ordering.Int.reverse)
          .grouped(2)
        // it takes 1 minute to move to new location, and one minute to open valve
        // so it takes 2 minutes to get new valve flowing (except if current one can be opened)
        val foldInitState =
          (
            openValveIds.contains(currentValveId) || openValveIds.contains(elephantValveId),
            timeLeft,
            0
          )
        val futureOpeningMax = remainingValves
          .foldLeft(foldInitState) {
            case (acc @ (firstIsOpen, timeLeftState, pressure), nxtGroup) =>
              if (timeLeftState <= 0) acc
              else
                val timePass = if (firstIsOpen) 1 else 2
                (
                  false,
                  timeLeftState - timePass,
                  pressure
                    + (nxtGroup.head * timeLeftState)
                    + (Try(nxtGroup(1)).getOrElse(0) * timeLeftState)
                )
          }
          ._3
        val result = pressureReleased + currentOpenPressures + futureOpeningMax
        bestTheoreticalFuturePressureForElimCache = Some(result)
        result

  case class Valve(id: String, rate: Int, routes: Seq[String])

  object Valve:
    def fromRaw(line: String): Valve =
      def cleanInput(id: String, rateInputStr: String, valveRoutes: Seq[String]): Valve =
        val rate   = rateInputStr.replace("rate=", "").replace(";", "").toInt
        val routes = valveRoutes.map(_.trim.replace(",", ""))
        Valve(id, rate, routes)

      line.split(" ").toList match {
        case "Valve" :: id :: "has" :: "flow" :: rateInput :: "tunnels" :: "lead" :: "to" :: "valves" :: valveRoutes =>
          cleanInput(id, rateInput, valveRoutes)
        case "Valve" :: id :: "has" :: "flow" :: rateInput :: "tunnel" :: "leads" :: "to" :: "valve" :: valveRoutes =>
          cleanInput(id, rateInput, valveRoutes)
      }

  def valvesToMap(valves: Seq[Valve]): ValveMap =
    valves.map(valve => valve.id -> valve).toMap

  def findBestRoute(
      state: TravelState,
      valveMap: ValveMap,
      resultRef: Ref[IO, Option[TravelState]],
      bestRouteRef: Ref[IO, Map[String, Int]],
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
      currentValve                   = valveMap(state.currentValveId)
      elephantValve                  = valveMap(state.elephantValveId)
      myNextMoveActions: Seq[Action] = currentValve.routes.map(routeId => Move(routeId))
      myNextActions: Seq[Action] =
        if (state.openValveIds.contains(state.currentValveId)) myNextMoveActions
        else myNextMoveActions :+ OpenValve()
      elephNextMoveActions: Seq[Action] = elephantValve.routes.map(routeId => Move(routeId))
      elepNextActions: Seq[Action] =
        if (state.openValveIds.contains(state.elephantValveId)) elephNextMoveActions
        else elephNextMoveActions :+ OpenValve()
      nextActionsCombined: Seq[(Action, Action)] = for {
        myAction    <- myNextActions
        elephAction <- elepNextActions
      } yield (myAction, elephAction)
      nextStates = nextActionsCombined.map((myAction, elephAction) =>
        state.doActions(myAction, elephAction, valveMap)
      )
      curBestState   <- resultRef.get
      statesWithHope <- filterBadStatesAndUpdateMap(nextStates, bestRouteRef)
      _ <-
        if (timeIsOut) IO.unit
        else if (
          state.bestTheoreticalFuturePressureForElim(valveMap) <= curBestState
            .map(_.pressureReleased)
            .getOrElse(0)
        ) IO.unit
        else if (statesWithHope.isEmpty) IO.unit
        else
          // Execute random job right away, put rest to the queue
          val nextStatesRandomized = Random.shuffle(statesWithHope)
          taskQueue.tryOfferN(nextStatesRandomized.tail)
            >> findBestRoute(
              nextStatesRandomized.head,
              valveMap,
              resultRef,
              bestRouteRef,
              taskQueue
            )
    } yield ()

  def filterBadStatesAndUpdateMap(
      states: Seq[TravelState],
      bestRouteRef: Ref[IO, Map[String, Int]]
  ): IO[List[TravelState]] =
    bestRouteRef.modify { routeMap =>
      val statesWithHope = states.filter { state =>
        val key = state.id
        routeMap.get(key) match {
          case None              => true
          case Some(curPressure) => curPressure >= state.pressureReleased
        }
      }
      val newMappings = statesWithHope
        .map(state => state.id -> state.pressureReleased)
      (routeMap ++ newMappings, statesWithHope.toList)
    }

  def findBestRouteSetup(
      initialState: TravelState,
      valveMap: ValveMap,
      parallelism: Int = 8
  ): IO[Option[TravelState]] =

    def processQueue(
        queue: PQueue[IO, TravelState],
        resultRef: Ref[IO, Option[TravelState]],
        bestRouteRef: Ref[IO, Map[String, Int]],
        iterations: Int
    ): IO[Unit] =

      def cleanUpQueue(): IO[Unit] =
        for {
          _                        <- IO(println(addThreadPrefix("Starting queue clean")))
          allStates                <- queue.tryTakeN(Some(Int.MaxValue))
          curBestState             <- resultRef.get
          statesWithBestRouteToLoc <- filterBadStatesAndUpdateMap(allStates, bestRouteRef)
          statesWithHope = statesWithBestRouteToLoc
            .filter(state =>
              state.bestTheoreticalFuturePressureForElim(valveMap) > curBestState
                .map(_.pressureReleased)
                .getOrElse(0)
            )
            .take(1_000_000)
          // statesWithHope = allStates.take(1_000_000)
          _ <- queue.tryOfferN(statesWithHope)
          oldSize = allStates.size
          newSize = statesWithHope.size
          _ <- IO(
            println(
              addThreadPrefix(
                s"Cleaned ${oldSize - newSize} states from queue, new size is $newSize"
              )
            )
          )
        } yield ()

      for {
        queueSize <- queue.size
        _ <-
          if (iterations != 0 && iterations % 10_000 == 0 && queueSize > 3_000_000) cleanUpQueue()
          else IO.unit
        states <- queue.tryTakeN(Some(parallelism))
        // Check what states should we traverse into
        // if some other route has already went through
        // the following combination curValveId - openedValveIds - timeLeft
        // but with more generated pressure, we should just stop.
        // Otherwise, we should update the mapping and continue
        statesToTraverse <- filterBadStatesAndUpdateMap(states, bestRouteRef)
        _ <- statesToTraverse
          .map(state => findBestRoute(state, valveMap, resultRef, bestRouteRef, queue))
          .parSequence
        queueSize <- queue.size
        _ <-
          if (iterations % 1_000_000 == 0)
            IO(println(addThreadPrefix(s"Queue size $queueSize after $iterations iterations")))
          else IO.unit
        _ <-
          if (queueSize == 0) IO.unit
          else processQueue(queue, resultRef, bestRouteRef, iterations + 1)
      } yield ()

    implicit val orderForTravelState: Order[TravelState] = Order.fromLessThan((state1, state2) =>
      val pressureWeight    = 0.2
      val futureValueWeight = 1.0 - pressureWeight

      def calcSortingScore(state: TravelState): Double =
        state.pressureReleased * pressureWeight + state.bestTheoreticalFuturePressureForSorting(
          valveMap
        ) * futureValueWeight

      calcSortingScore(state1) > calcSortingScore(state2)
    )

    for {
      _ <- IO.unit
      initialRes: Option[TravelState] = None
      bestRouteRef <- Ref[IO].of(initialRes)
      initalMapRef: Map[String, Int] = Map.empty
      bestRouteByTimeRef <- Ref[IO].of(initalMapRef)
      taskQueue          <- PQueue.unbounded[IO, TravelState] // (1_000_000)
      _                  <- taskQueue.offer(initialState)
      _                  <- processQueue(taskQueue, bestRouteRef, bestRouteByTimeRef, 0)
      result             <- bestRouteRef.get
    } yield result

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    valves = input.split("\n").map(Valve.fromRaw)
  } yield valves.toSeq

  /**
   * Unfortunately, this will not work with every execution with the bigger input, as the randomness
   * can cause the algorithm to go to a bad state.
   * Even if it finds the correct result, it will just print it, and the execution will continue.
   * My functions optimizing early exists are bad.. :)
   */
  override def run = for {
    valves <- parseFile()
    _      <- IO(println(valves))
    valveMap = valvesToMap(valves)
    initialState = TravelState(
      currentValveId = "AA",
      elephantValveId = "AA",
      openValveIds = Set.empty,
      pressureReleased = 0,
      timeLeft = 26
    )
    bestRouteState <- findBestRouteSetup(
      initialState,
      valveMap,
      parallelism = Runtime.getRuntime.availableProcessors()
    )
    _ <- IO(println(bestRouteState))
  } yield ()
}
