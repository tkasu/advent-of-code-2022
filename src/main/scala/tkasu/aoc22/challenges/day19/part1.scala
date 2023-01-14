package tkasu.aoc22.challenges.day19

import scala.util.matching.Regex
import cats.Order
import cats.implicits.*
import cats.effect.{IO, IOApp, Ref}
import cats.effect.std.PQueue
import tkasu.aoc22.utils.string.*
import tkasu.aoc22.utils.files.{makeSourceResource, readLines}

import scala.util.Random

object part1 extends IOApp.Simple {

  // private val inputResource = makeSourceResource("day19/part1/input_example")
  private val inputResource = makeSourceResource("day19/part1/input")

  val END_MINUTE: Int = 24

  given orderForTravelState: Order[State] = Order.fromLessThan((state1, state2) =>
    state1.queueRank > state2.queueRank
  )
  given parallelism: BatchSize = BatchSize(100)

  case class BatchSize(value: Int)

  type MineralCounts = Map[Mineral, Int]
  type Robots        = Map[Mineral, Int]

  enum Mineral:
    case Ore, Clay, Obsidian, Geode

  enum Action:
    case Wait
    case Robot(mineral: Mineral)

  object Action:
    def values: List[Action] =
      Wait :: Mineral.values.map(mineral => Action.Robot(mineral)).toList

  case class State(
      minute: Int,
      actions: Seq[Action],
      blueprint: Blueprint,
      robots: Robots,
      minerals: MineralCounts
  ):

    lazy val timeLeft: Int  = END_MINUTE - minute
    lazy val queueRank: Int = minute + minerals(Mineral.Geode)

    def doWaitAction(times: Int = 1): State =
      val production: MineralCounts = robots.map { case (mineral, production) =>
        mineral -> production * times
      }
      this.copy(
        minute = minute + times,
        actions = actions ++ (1 to times).map(_ => Action.Wait),
        minerals = minerals.combine(production)
      )

    def doAction(action: Action): State =
      val newRobots = action match {
        case Action.Wait           => robots
        case Action.Robot(mineral) => robots + (mineral -> (robots(mineral) + 1))
      }
      val newMineralsAfterBuild = action match {
        case Action.Wait => minerals
        case Action.Robot(mineral) =>
          blueprint.getRobotCostFrom(mineral).mineralsAfterBuild(minerals)
      }
      this.copy(
        minute = minute + 1,
        actions = actions :+ action,
        minerals = newMineralsAfterBuild.combine(robots),
        robots = newRobots
      )

    def nextRobotStates(): Seq[State] =
      val robotActions = Action.values.toSet - Action.Wait

      val robotResourceDeficits: Seq[(Mineral, MineralCounts)] = robotActions.map {
        case Action.Robot(mineral) =>
          val resourceDeficits: MineralCounts = blueprint
            .getRobotCostFrom(mineral)
            .mineralsAfterBuild(minerals)
            .map { case (k, v) => k -> (if (v > 0) 0 else v * -1) }
          mineral -> resourceDeficits
      }.toSeq

      val minutesUntilCanBuild: Seq[(Mineral, Option[Int])] = robotResourceDeficits.map {
        case (mineral, deficits) =>
          val productionMinutes = deficits.map { case (mineral, deficit) =>
            val production = robots(mineral)
            if (deficit == 0) Some(0)
            else if (production == 0) None
            else
              Some(Math.ceil(deficit.toDouble / production.toDouble).toInt)
          }
          if (productionMinutes.flatten.isEmpty) mineral -> None
          else if (productionMinutes.exists(_.isEmpty)) mineral -> None
          else mineral -> Some(productionMinutes.flatten.max)
      }

      val states = minutesUntilCanBuild.flatMap { case (mineral, minutesOption) =>
        minutesOption match {
          case None => None
          case Some(minutes) =>
            if (minutes > timeLeft) None
            else if (minutes == 0)
              val action = Action.Robot(mineral)
              if !(this.canDo(action)) then
                throw IllegalAccessException(s"Fatal error, can't build $action with $minerals")
              else Some(this.doAction(action))
            else Some(this.doWaitAction(minutes))
        }
      }
      if (states.nonEmpty) states else Seq(this.doWaitAction(timeLeft))

    lazy val theoreticalBestGeode: Int =
      val currentGeode            = minerals(Mineral.Geode)
      val currentRobotsProduction = robots(Mineral.Geode) * timeLeft
      currentGeode + currentRobotsProduction + ((timeLeft - 1) * timeLeft / 2)

    private def canDo(action: Action): Boolean =
      action match {
        case Action.Wait => true
        case Action.Robot(mineral) =>
          blueprint
            .getRobotCostFrom(mineral)
            .canBuild(minerals)
      }

  object State:

    def fromBlueprint(blueprint: Blueprint): State =
      State(
        minute = 0,
        Seq.empty,
        blueprint,
        robots = Map(
          Mineral.Ore      -> 1,
          Mineral.Clay     -> 0,
          Mineral.Obsidian -> 0,
          Mineral.Geode    -> 0
        ),
        minerals = Map(
          Mineral.Ore      -> 0,
          Mineral.Clay     -> 0,
          Mineral.Obsidian -> 0,
          Mineral.Geode    -> 0
        )
      )

  case class RobotCost(
      oreCost: Int,
      clayCost: Int,
      obsidianCost: Int
  ):

    def canBuild(minerals: MineralCounts): Boolean =
      if (minerals(Mineral.Ore) < oreCost) false
      else if (minerals(Mineral.Clay) < clayCost) false
      else if (minerals(Mineral.Obsidian) < obsidianCost) false
      else true

    def mineralsAfterBuild(minerals: MineralCounts): MineralCounts =
      Map(
        Mineral.Ore      -> (minerals(Mineral.Ore) - oreCost),
        Mineral.Clay     -> (minerals(Mineral.Clay) - clayCost),
        Mineral.Obsidian -> (minerals(Mineral.Obsidian) - obsidianCost),
        Mineral.Geode    -> minerals(Mineral.Geode)
      )

  case class Blueprint(
      id: Int,
      oreRobotCost: RobotCost,
      clayRobotCost: RobotCost,
      obsidianRobotCost: RobotCost,
      geodeRobotCost: RobotCost
  ):

    def getRobotCostFrom(mineral: Mineral): RobotCost =
      mineral match {
        case Mineral.Ore      => oreRobotCost
        case Mineral.Clay     => clayRobotCost
        case Mineral.Obsidian => obsidianRobotCost
        case Mineral.Geode    => geodeRobotCost
      }

  object Blueprint:

    private val blueprintPattern: Regex =
      """^Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.$""".r

    def fromRaw(line: String): Blueprint =
      line match {
        case blueprintPattern(
              id,
              oreRobotOreCost,
              clayRobotOreCost,
              obsidianRobotOreCost,
              obsidianRobotClayCost,
              geaodeRobotOreCost,
              geaodeRobotObsidianCost
            ) =>
          Blueprint(
            id = id.toInt,
            oreRobotCost = RobotCost(
              oreCost = oreRobotOreCost.toInt,
              clayCost = 0,
              obsidianCost = 0
            ),
            clayRobotCost = RobotCost(
              oreCost = clayRobotOreCost.toInt,
              clayCost = 0,
              obsidianCost = 0
            ),
            obsidianRobotCost = RobotCost(
              oreCost = obsidianRobotOreCost.toInt,
              clayCost = obsidianRobotClayCost.toInt,
              obsidianCost = 0
            ),
            geodeRobotCost = RobotCost(
              oreCost = geaodeRobotOreCost.toInt,
              clayCost = 0,
              obsidianCost = geaodeRobotObsidianCost.toInt
            )
          )
        case _ => throw IllegalAccessException(s"Cant parse line '$line'")
      }

  def findNextStates(
      state: State,
      resultRef: Ref[IO, Option[State]],
      stateQueue: PQueue[IO, State]
  ): IO[Unit] =
    for {
      _ <- IO.unit
      reachedEnd = state.minute >= END_MINUTE
      currentBestState <- resultRef.get
      _ <-
        if (reachedEnd)
          for {
            _ <- IO.unit
            _ <-
              resultRef
                .modify {
                  case None => (Some(state), ())
                  case Some(oldBest) =>
                    if (state.minerals(Mineral.Geode) > oldBest.minerals(Mineral.Geode))
                      println(
                        addThreadPrefix(
                          s"Found new best solution for Blueprint: ${state.blueprint.id}, Geodes: ${state
                              .minerals(Mineral.Geode)}"
                        )
                      )
                      (Some(state), ())
                    else (Some(oldBest), ())
                }
          } yield ()
        else IO.unit
      canReachedBest = currentBestState match {
        case None            => true
        case Some(bestState) => state.theoreticalBestGeode > bestState.minerals(Mineral.Geode)
      }
      possibleNextStates = state.nextRobotStates()
      _ <- IO.unit
      _ <-
        if (reachedEnd || !canReachedBest) IO.unit
        else
          stateQueue.tryOfferN(possibleNextStates.toList)
          // Execute random job right away, put rest to the queue
          /*
          val nextStatesRandomized = Random.shuffle(possibleNextStates)
          stateQueue.tryOfferN(nextStatesRandomized.tail.toList)
              >> IO.cede
              >> findNextStates(nextStatesRandomized.head, resultRef, stateQueue)
                .guarantee(IO.cede)
          */

    } yield ()

  def processQueue(
      resultRef: Ref[IO, Option[State]],
      stateQueue: PQueue[IO, State]
  )(using parallelism: BatchSize): IO[Unit] =
    for {
      _          <- IO.unit
      nextStates <- stateQueue.tryTakeN(Some(parallelism.value))
      _         <- nextStates.map(state => findNextStates(state, resultRef, stateQueue)).parSequence
      queueSize <- stateQueue.size
      // _ <- IO(println(queueSize))
      _ <-
        if (queueSize == 0) IO.unit
        else processQueue(resultRef, stateQueue)
    } yield ()

  def findOptimalState(state: State): IO[Option[State]] =
    for {
      _ <- IO.unit
      initialRefValue: Option[State] = None
      resultRef    <- Ref[IO].of(initialRefValue)
      stateQueue   <- PQueue.unbounded[IO, State]
      -            <- stateQueue.offer(state)
      _            <- processQueue(resultRef, stateQueue)
      optimalState <- resultRef.get
    } yield optimalState

  def optimalStates(blueprints: Seq[Blueprint]): IO[Seq[Option[State]]] =
    for {
      _ <- IO.unit
      initialStates = blueprints.map(State.fromBlueprint)
      optimalStates <- initialStates
        .map(findOptimalState)
        .parSequence
    } yield optimalStates

  def qualityLevels(states: Seq[State]): Seq[Int] =
    states.map(state => state.blueprint.id * state.minerals(Mineral.Geode))

  def parseFile() = for {
    input <- inputResource.use(src => readLines(src))
    blueprints = input.split("\n").map(Blueprint.fromRaw)
  } yield blueprints.toSeq

  /**
   * This solution is still awfully slow and sub optimal, but gets the job done if given some time.
   * I struggled optimizing the solution, and so I took inspiration from the following repo to the
   * following sections: https://github.com/polarfish/advent-of-code-2022/blob/main/src/main/java/Day19.java
   * - Jump into next possible build states instead of always just going to next minute with all possible states
   *   (including wait)
   * - Last part of the formula for theoreticalBestGeode
   */
  override def run = for {
    blueprints    <- parseFile()
    _             <- IO(println(blueprints))
    optimalStates <- optimalStates(blueprints)
    _             <- IO(println(optimalStates))
    qualityScore = qualityLevels(optimalStates.flatten).sum
    _ <- IO(println(s"Quality score: $qualityScore"))
  } yield ()
}
