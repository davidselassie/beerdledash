import GameTypes.Name
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directive, Directive1}
import akka.http.scaladsl.server.Directives.{
  complete,
  formFields,
  onSuccess,
  parameters,
  tprovide
}
import akka.util.Timeout
import scalatags.Text.all._

object GameDirective {
  import ScalatagsMarshallers._
  import akka.actor.typed.scaladsl.AskPattern.{
    Askable,
    schedulerFromActorSystem
  }

  private def gameNotFoundBody(code: String) = doctype("html")(
    html(lang := "en")(
      head(
        meta(
          name := "viewport",
          content := "width=device-width, initial-scale=1"
        ),
        link(rel := "stylesheet", href := "static/site.css"),
        link(rel := "icon", href := "static/favicon.png"),
        tag("title")("Beerdledash")
      ),
      body(
        h1("No Game ", code),
        tag("main")(id := "main")(
          p("There's no game with that code... yet."),
          p(
            "Go back to the ",
            a(href := "/")("main page"),
            " and make a new game."
          )
        )
      )
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
