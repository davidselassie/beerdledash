import GameTypes.Code
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

import scala.util.Random

object Directory {

  sealed trait Msg
  final case class Create(replyTo: ActorRef[Created]) extends Msg
  final case class Find(replyTo: ActorRef[FindRes], code: Code) extends Msg
  private final case class Unregister(code: Code) extends Msg

  final case class Created(code: Code, room: ActorRef[Game.Msg])

  sealed trait FindRes
  final case class Found(game: ActorRef[Game.Msg]) extends FindRes
  case object NotFound extends FindRes

  def apply(random: Random): Behavior[Msg] = Behaviors.setup { (ctx) =>
    ctx.log.info("Directory starting")
    new Directory(ctx, random).behavior(Map.empty)
  }
}

private class Directory(ctx: ActorContext[Directory.Msg], random: Random) {
  import Directory._

  def behavior(
      phonebook: Map[Code, ActorRef[Game.Msg]]
  ): Behavior[Msg] = Behaviors.receiveMessage {
    case Create(replyTo) => {
      val newCode = genCodes()
        .filterNot((code) => phonebook.contains(code))
        .head
      val game = ctx.spawn(Game(random), newCode.toString)
      ctx.watchWith(game, Unregister(newCode))
      replyTo ! Created(newCode, game)
      ctx.log.info(s"Created game ${newCode}")
      behavior(phonebook.updated(newCode, game))
    }
    case Find(replyTo, code) => {
      phonebook.get(code) match {
        case Some(game) => {
          replyTo ! Found(game)
          ctx.log.debug(s"Found game ${code}")
        }
        case None => {
          replyTo ! NotFound
          ctx.log.debug(s"Didn't find game ${code}")
        }
      }
      Behaviors.same
    }
    case Unregister(code) => {
      ctx.log.debug(s"Unregistered game ${code}")
      behavior(phonebook.removed(code))
    }
  }

  private def genCode(): Code =
    Code(
      random.alphanumeric
        .filter(_.isLetter)
        .take(Code.Length)
        .mkString
        .toUpperCase
    )

  private def genCodes(): LazyList[Code] = {
    genCode() #:: genCodes()
  }
}
