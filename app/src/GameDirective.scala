import GameRejection.RoomNotFoundRejection
import GameTypes.{Code, Name}
import GameTypesUnmarshallers._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.server.Directives.{
  formFields,
  onSuccess,
  parameters,
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

  def formGame(implicit
      directory: ActorRef[Directory.Msg],
      system: ActorSystem[_],
      timeout: Timeout
  ): Directive[(Code, ActorRef[Game.Msg], Game.State)] =
    formFields("code".as[Code]).flatMap((code) => find(code))

  def queryGame(implicit
      directory: ActorRef[Directory.Msg],
      system: ActorSystem[_],
      timeout: Timeout
  ): Directive[(Code, ActorRef[Game.Msg], Game.State)] =
    parameters("code".as[Code]).flatMap((code) => find(code))

  val formPlayer: Directive1[Name] = formFields("name".as[Name])

  val queryPlayer: Directive1[Name] = parameters("name".as[Name])
}
