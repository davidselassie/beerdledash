import GameTypes._
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop}
import akka.stream.scaladsl.SourceQueueWithComplete

import scala.util.Random

object Game {

  final case class Transition(
      msg: Msg,
      newState: State
  )

  sealed trait Msg
  object Msg {

    final case class GetState(replyTo: ActorRef[State]) extends Msg

    final case class WatchState(watcher: SourceQueueWithComplete[Transition])
        extends Msg

    final case class UnwatchState(watcher: SourceQueueWithComplete[Transition])
        extends Msg

    final case class PlayerJoin(name: Name) extends Msg

    final case class AssignBeer(name: Name, beer: Beer) extends Msg

    case object StartGame extends Msg

    final case class SubmitDesc(
        roundNum: RoundNum,
        writer: Name,
        desc: Desc
    ) extends Msg

    final case class ReadNext(
        roundNum: RoundNum,
        lastIndex: BallotIndex
    ) extends Msg

    final case class RecordVote(
        roundNum: RoundNum,
        voter: Name,
        index: BallotIndex
    ) extends Msg

    final case class BeginNextRound(
        lastRoundNum: RoundNum
    ) extends Msg

    case object Close extends Msg
  }

  sealed trait State
  object State {

    final case class WaitingState(
        leader: Option[Name],
        canStart: Boolean,
        playerBeers: Map[Name, Option[Beer]]
    ) extends State

    final case class WritingState(
        leader: Name,
        roundNum: RoundNum,
        host: Name,
        beer: Beer,
        descs: Map[Name, Option[Desc]],
        roundsRemaining: Seq[Round],
        roundLog: Seq[RoundRecord]
    ) extends State

    final case class ReadingState(
        leader: Name,
        roundNum: RoundNum,
        host: Name,
        beer: Beer,
        entry: BallotEntry,
        entriesRead: Seq[BallotEntry],
        entriesRemaining: Seq[BallotEntry],
        roundsRemaining: Seq[Round],
        roundLog: Seq[RoundRecord]
    ) extends State

    final case class VotingState(
        leader: Name,
        roundNum: RoundNum,
        host: Name,
        beer: Beer,
        ballot: IndexedSeq[BallotEntry],
        votes: Map[Name, Option[BallotIndex]],
        roundsRemaining: Seq[Round],
        roundLog: Seq[RoundRecord]
    ) extends State

    final case class FinRoundState(
        leader: Name,
        thisRoundRecord: RoundRecord,
        roundsRemaining: Seq[Round],
        roundLog: Seq[RoundRecord]
    ) extends State

    final case class FinRoomState(
        finalScores: Map[Name, Int],
        roundLog: Seq[RoundRecord]
    ) extends State

  }

  private val InitState = State.WaitingState(None, canStart = false, Map.empty)

  def apply(random: Random): Behavior[Msg] = Behaviors.setup { (ctx) =>
    val timeoutMonitor = ctx.spawn(GameMonitor(ctx.self), "timeout")
    val initBehavior = new Game(random, ctx).waitingBehavior(InitState)
    ctx.log.info("Starting room")
    Behaviors.monitor(timeoutMonitor, initBehavior)
  }
}

private class Game(
    random: Random,
    ctx: ActorContext[Game.Msg]
) {
  import Game._

  private var watchers = Set.empty[SourceQueueWithComplete[Transition]]

  private def transitionTo[T <: State](
      behavior: (T) => Behavior[Msg],
      msg: Msg,
      newState: T
  ): Behavior[Msg] = {
    broadcast(msg, newState)
    behavior(newState)
  }

  private def broadcast(msg: Msg, newState: State): Unit = {
    val transition = Transition(msg, newState)
    for (watcher <- watchers) {
      watcher.offer(transition)
    }
  }

  private def defaultHandler(
      state: State
  ): PartialFunction[Msg, Behavior[Msg]] = {
    case Msg.GetState(replyTo) => {
      replyTo ! state
      Behaviors.same
    }
    case Msg.WatchState(watcher) => {
      watchers = watchers + watcher
      Behaviors.same
    }
    case Msg.UnwatchState(watcher) => {
      watchers = watchers - watcher
      Behaviors.same
    }
    case Msg.Close => Behaviors.stopped
    case _         => Behaviors.same
  }

  private def behaviorFor[T <: State](
      handler: T => PartialFunction[Msg, Behavior[Msg]],
      state: T
  ): Behavior[Msg] = {
    Behaviors
      .receiveMessagePartial(handler(state).orElse(defaultHandler(state)))
      .receiveSignal {
        case (_, PostStop) => {
          for (watcher <- watchers) {
            watcher.complete()
          }
          watchers = Set.empty
          Behaviors.same
        }
      }
  }

  def waitingBehavior(state: State.WaitingState) =
    behaviorFor(waitingHandler, state)

  private def canStart(
      leader: Option[Name],
      playerBeers: Map[Name, Option[Beer]]
  ): Boolean = {
    leader.isDefined && playerBeers.size > 2 && playerBeers.values.forall(
      _.nonEmpty
    )
  }

  private def waitingHandler(
      state: State.WaitingState
  ): PartialFunction[Msg, Behavior[Msg]] = {
    case msg @ Msg.PlayerJoin(name) => {
      if (!state.playerBeers.contains(name)) {
        val newLeader = Some(state.leader.getOrElse(name))
        val newPlayerBeers = state.playerBeers.updated(name, None)
        val newState = state.copy(
          leader = newLeader,
          canStart = canStart(newLeader, newPlayerBeers),
          playerBeers = newPlayerBeers
        )
        transitionTo(waitingBehavior, msg, newState)
      } else {
        Behaviors.same
      }
    }
    case msg @ Msg.AssignBeer(name, beer) => {
      val newPlayerBeers = state.playerBeers.updated(name, Some(beer))
      val newState =
        state.copy(
          canStart = canStart(state.leader, newPlayerBeers),
          playerBeers = newPlayerBeers
        )
      transitionTo(waitingBehavior, msg, newState)
    }
    case msg @ Msg.StartGame if canStart(state.leader, state.playerBeers) => {
      val playerBeers = state.playerBeers.flatten {
        case (name, Some(beer)) => Seq((name, beer))
        case (_, None)          => Seq.empty
      }
      val rounds = for {
        ((host, beer), i) <- random.shuffle(playerBeers).zipWithIndex
      } yield {
        Round(RoundNum(i + 1), host, beer)
      }
      (state.leader, rounds) match {
        case (Some(leader), firstRound :: roundsRemaining) => {
          val newState = State.WritingState(
            leader,
            firstRound.num,
            firstRound.host,
            firstRound.beer,
            state.playerBeers.keySet.map((_, None)).toMap,
            roundsRemaining,
            Seq.empty
          )
          transitionTo(writingBehavior, msg, newState)
        }
        case _ => {
          val newState = State.FinRoomState(
            Map.empty,
            Seq.empty
          )
          transitionTo(finRoomBehavior, msg, newState)
        }
      }
    }
  }

  private def writingBehavior(state: State.WritingState) =
    behaviorFor(writingHandler, state)

  private def writingHandler(
      state: State.WritingState
  ): PartialFunction[Msg, Behavior[Msg]] = {
    case msg @ Msg.SubmitDesc(roundNum, writer, desc)
        if roundNum == state.roundNum && state.descs.contains(writer) => {
      val newDescs = state.descs.updated(writer, Some(desc))
      if (newDescs.values.exists(_.isEmpty)) {
        val newState = state.copy(descs = newDescs)
        transitionTo(writingBehavior, msg, newState)
      } else {
        val writerToDesc = newDescs.flatten {
          case (name, Some(desc)) => Seq((name, desc))
          case (_, None)          => Seq.empty
        }
        val ballot = for {
          ((writer, desc), i) <- random.shuffle(writerToDesc).zipWithIndex
        } yield {
          BallotEntry(BallotIndex(i + 1), desc, writer)
        }
        val newState = State.ReadingState(
          state.leader,
          state.roundNum,
          state.host,
          state.beer,
          ballot.head,
          Seq.empty,
          ballot.tail.toSeq,
          state.roundsRemaining,
          state.roundLog
        )
        transitionTo(readingBehavior, msg, newState)
      }
    }
  }

  private def readingBehavior(state: State.ReadingState) =
    behaviorFor(readingHandler, state)

  private def readingHandler(
      state: State.ReadingState
  ): PartialFunction[Msg, Behavior[Msg]] = {
    case msg @ Msg.ReadNext(roundNum, lastIndex)
        if roundNum == state.roundNum && lastIndex == state.entry.index => {
      state.entriesRemaining match {
        case nextEntry :: newEntriesRemaining => {
          val newState = state.copy(
            entry = nextEntry,
            entriesRead = state.entriesRead :+ state.entry,
            entriesRemaining = newEntriesRemaining
          )
          transitionTo(readingBehavior, msg, newState)
        }
        case Seq() => {
          val ballot = state.entriesRead :+ state.entry
          val voters = ballot.map(_.writer).toSet - state.host
          if (voters.size > 1) {
            val newState = State.VotingState(
              state.leader,
              state.roundNum,
              state.host,
              state.beer,
              ballot.toIndexedSeq,
              voters.map((voter) => (voter, None)).toMap,
              state.roundsRemaining,
              state.roundLog
            )
            transitionTo(votingBehavior, msg, newState)
          } else {
            val newState = State.FinRoundState(
              state.leader,
              RoundRecord(
                state.roundNum,
                state.beer,
                ScoredBallotEntry(
                  state.entry.index,
                  state.entry.desc,
                  state.entry.writer,
                  Set.empty
                ),
                Seq.empty,
                Map(state.host -> 3)
              ),
              state.roundsRemaining,
              state.roundLog
            )
            transitionTo(finRoundBehavior, msg, newState)
          }
        }
      }
    }
  }

  private def votingBehavior(state: State.VotingState) =
    behaviorFor(votingHandler, state)

  private def votingHandler(
      state: State.VotingState
  ): PartialFunction[Msg, Behavior[Msg]] = {
    case msg @ Msg.RecordVote(roundNum, voter, index)
        if roundNum == state.roundNum && state.votes.contains(voter) => {
      val newVotes = state.votes.updated(voter, Some(index))
      if (newVotes.values.exists(_.isEmpty)) {
        val newState = state.copy(votes = newVotes)
        transitionTo(votingBehavior, msg, newState)
      } else {
        val votes = newVotes.flatMap {
          case (voter, Some(i)) => Seq((voter, i))
          case (_, None)        => Seq.empty
        }
        val voterToWriter = votes.map { case (voter, i) =>
          (voter, state.ballot(i.toInt - 1).writer)
        }
        val writerToVoters = voterToWriter
          .groupMapReduce { case (voter, writer) => writer } {
            case (voter, writer) => Set(voter)
          }((v, acc) => v ++ acc)

        val zeroFillPoints = state.ballot.map((be) => (be.writer, 0)).toMap
        val pickedTruthPoints = writerToVoters
          .getOrElse(state.host, Set.empty)
          .map(voter => (voter, 2))
          .toMap
        val nobodyPickedTruthPoints = if (pickedTruthPoints.isEmpty) {
          Map(state.host -> 3)
        } else {
          Map.empty[Name, Int]
        }
        val fakedOutPoints = writerToVoters
          .removed(state.host)
          .map { case (writer, voters) =>
            (writer, voters.size * 1)
          }
        val nameToScore = Seq(
          zeroFillPoints,
          pickedTruthPoints,
          nobodyPickedTruthPoints,
          fakedOutPoints
        ).flatten
          .groupMapReduce({ case (name, score) => name })({
            case (name, score) => score
          })(_ + _)

        val (trueEntry, fakeEntries) = state.ballot
          .map(be =>
            ScoredBallotEntry(
              be.index,
              be.desc,
              be.writer,
              writerToVoters.getOrElse(be.writer, Set.empty)
            )
          )
          .partition(_.writer == state.host)

        val newState = State.FinRoundState(
          state.leader,
          RoundRecord(
            state.roundNum,
            state.beer,
            trueEntry.head,
            fakeEntries,
            nameToScore
          ),
          state.roundsRemaining,
          state.roundLog
        )
        transitionTo(finRoundBehavior, msg, newState)
      }
    }
  }

  private def finRoundBehavior(state: State.FinRoundState) =
    behaviorFor(finRoundHandler, state)

  private def finRoundHandler(
      state: State.FinRoundState
  ): PartialFunction[Msg, Behavior[Msg]] = {
    case msg @ Msg.BeginNextRound(lastRoundNum)
        if lastRoundNum == state.thisRoundRecord.roundNum => {
      val newRoundLog = state.roundLog :+ state.thisRoundRecord
      state.roundsRemaining match {
        case nextRound :: newRoundsRemaining => {
          val newState = State.WritingState(
            state.leader,
            nextRound.num,
            nextRound.host,
            nextRound.beer,
            state.thisRoundRecord.roundScores.keys
              .map((name) => (name, None))
              .toMap,
            newRoundsRemaining,
            newRoundLog
          )
          transitionTo(writingBehavior, msg, newState)
        }
        case Seq() => {
          val scores = newRoundLog
            .map(_.roundScores)
            .flatten
            .groupMapReduce({ case (name, score) => name })({
              case (name, score) => score
            })(_ + _)
          val newState = State.FinRoomState(
            scores,
            newRoundLog
          )
          transitionTo(finRoomBehavior, msg, newState)
        }
      }
    }
  }

  private def finRoomBehavior(state: State.FinRoomState) =
    behaviorFor(finRoomHandler, state)

  private def finRoomHandler(
      state: State.FinRoomState
  ): PartialFunction[Msg, Behavior[Msg]] = PartialFunction.empty
}
