import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

import scala.concurrent.duration.{FiniteDuration, HOURS}

object GameMonitor {

  private val Timeout = FiniteDuration(1, HOURS)

  def apply(room: ActorRef[Game.Msg]): Behavior[Game.Msg] = Behaviors.setup {
    (ctx) =>
      ctx.log.info("Monitor starting")
      val timer = ctx.spawn(Timer(Timeout, room, Game.Msg.Close), "timer")
      new GameMonitor(ctx, timer).monitor()
  }
}

private class GameMonitor(
    ctx: ActorContext[_],
    timer: ActorRef[Timer.Msg]
) {

  def monitor(): Behavior[Game.Msg] = Behaviors.receiveMessage {
    case _: Game.Msg.GetState | _: Game.Msg.WatchState |
        _: Game.Msg.UnwatchState =>
      Behaviors.same // Don't reset timeout on management messages.
    case Game.Msg.Close => Behaviors.stopped
    case _ => {
      timer ! Timer.Reset
      Behaviors.same
    }
  }
}
