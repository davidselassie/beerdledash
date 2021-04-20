import GameTypes.Name
import HtmlRenderer.htmlContent
import ScalatagsMarshallers._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{
  complete,
  formFields,
  onSuccess,
  parameters,
  tprovide
}
import akka.http.scaladsl.server.{Directive, Directive1}
import akka.util.Timeout
import scalatags.Text.attrs.{href, id}
import scalatags.Text.implicits._
import scalatags.Text.tags.{a, h1, p, code => codetag}
import scalatags.Text.tags2.main

object GameDirective {
  import akka.actor.typed.scaladsl.AskPattern.{
    Askable,
    schedulerFromActorSystem
  }

  private def gameNotFoundBody(code: String) = htmlContent(
    main(
      h1("No Game ", codetag(code)),
      p("There's no game with code ", codetag(code), ".")
    )
  )

  private def find(code: String)(implicit
      directory: ActorRef[Directory.Msg],
      system: ActorSystem[_],
      timeout: Timeout
  ): Directive[(String, ActorRef[Game.Msg], Game.State)] = onSuccess(
    directory.ask((replyTo) => Directory.Find(replyTo, code))
  ).flatMap {
    case Directory.Found(game) => {
      onSuccess(game.ask((replyTo) => Game.Msg.GetState(replyTo))).flatMap {
        (state) =>
          tprovide((code, game, state))
      }
    }
    case Directory.NotFound =>
      complete(StatusCodes.NotFound, gameNotFoundBody(code))
  }

  def formGame(implicit
      directory: ActorRef[Directory.Msg],
      system: ActorSystem[_],
      timeout: Timeout
  ): Directive[(String, ActorRef[Game.Msg], Game.State)] =
    formFields("code").flatMap((code) => find(code))

  def queryGame(implicit
      directory: ActorRef[Directory.Msg],
      system: ActorSystem[_],
      timeout: Timeout
  ): Directive[(String, ActorRef[Game.Msg], Game.State)] =
    parameters("code").flatMap((code) => find(code))

  val formPlayer: Directive1[Name] =
    formFields("name").map((name) => Name(name))

  val queryPlayer: Directive1[Name] =
    parameters("name").map((name) => Name(name))
}
