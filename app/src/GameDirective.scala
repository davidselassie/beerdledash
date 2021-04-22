import GameRejection.RoomNotFoundRejection
import GameTypes.{Code, Name}
import GameTypesUnmarshallers._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.server.Directives.{
  onSuccess,
  parameter,
  reject,
  tprovide
}
import akka.http.scaladsl.server.directives.FormFieldDirectives._
import akka.http.scaladsl.server.{Directive, Directive1}
import akka.util.Timeout

object GameDirective {
  import akka.actor.typed.scaladsl.AskPattern.{
    Askable,
    schedulerFromActorSystem
  }

  private def find(code: Code)(implicit
      directory: ActorRef[Directory.Msg],
      system: ActorSystem[_],
      timeout: Timeout
  ): Directive[(Code, ActorRef[Game.Msg], Game.State)] = onSuccess(
    directory.ask((replyTo) => Directory.Find(replyTo, code))
  ).flatMap {
    case Directory.Found(game) => {
      onSuccess(game.ask((replyTo) => Game.Msg.GetState(replyTo))).flatMap {
        (state) =>
          tprovide((code, game, state))
      }
    }
    case Directory.NotFound => reject(RoomNotFoundRejection(code))
  }

  def gameParam(implicit
      directory: ActorRef[Directory.Msg],
      system: ActorSystem[_],
      timeout: Timeout
  ): Directive[(Code, ActorRef[Game.Msg], Game.State)] =
    parameter("code".as[Code]).flatMap((code) => find(code))

  val playerParam: Directive1[Name] = parameter("name".as[Name])
}
