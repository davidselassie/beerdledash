import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}

import scala.concurrent.duration.FiniteDuration

object Timer {
  sealed trait Msg
  case object Reset extends Msg
  private case object Fire extends Msg

  def apply[T](
      delay: FiniteDuration,
      target: ActorRef[T],
      fireMsg: T
  ): Behavior[Msg] = Behaviors.setup { (ctx) =>
    Behaviors.withTimers { (timers) =>
      timers.startSingleTimer(this, Fire, delay)
      new Timer(ctx, timers, delay, target, fireMsg).behavior()
    }
  }
}

private class Timer[T](
    ctx: ActorContext[_],
    timers: TimerScheduler[Timer.Msg],
    delay: FiniteDuration,
    target: ActorRef[T],
    fireMsg: T
) {
  import Timer._

  def behavior(): Behavior[Msg] = Behaviors.receiveMessage {
    case Reset => {
      timers.startSingleTimer(this, Fire, delay)
      Behaviors.same
    }
    case Fire => {
      target ! fireMsg
      timers.cancel(this)
      Behaviors.stopped
    }
  }
}
