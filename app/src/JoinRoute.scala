import Extensions.QueryStringEx
import GameDirective.{formPlayer, queryGame}
import GameTypes.Name
import HtmlRenderer.htmlContent
import ScalatagsMarshallers._
import akka.actor.typed.scaladsl.AskPattern.{Askable, schedulerFromActorSystem}
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{
  complete,
  concat,
  get,
  onSuccess,
  path,
  pathPrefix,
  post,
  redirect
}
import akka.http.scaladsl.server.Route
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import com.google.zxing.qrcode.encoder.Encoder
import scalatags.Text.attrs.{`type`, method, name, placeholder, required, value}
import scalatags.Text.implicits._
import scalatags.Text.tags.{form, h1, input, label}
import scalatags.Text.tags2.main

import java.net.URL

class JoinRoute(
    val publicRoot: URL,
    directory: ActorRef[Directory.Msg]
)(implicit
    system: ActorSystem[_]
) extends RouteObj {

  override val route: Route = pathPrefix("join") {
    concat(
      path("qr") {
        get {
          handleQr()
        }
      },
      get(handleGet()),
      post(handlePost())
    )
  }

  private def handleGet() = queryGame(directory, system, askTimeout) {
    (code, room, state) =>
      state match {
        case _: Game.State.WaitingState =>
          complete(
            StatusCodes.OK,
            htmlContent(
              main(
                h1("Join Game ", code),
                form(method := "POST")(
                  label(
                    "What's your name? ",
                    input(
                      `type` := "text",
                      name := "name",
                      required,
                      placeholder := "Rob"
                    )
                  ),
                  input(`type` := "submit", value := "Join")
                )
              )
            )
          )
        case _ =>
          complete(
            StatusCodes.BadRequest,
            "Game already in progress. Can't join."
          )
      }

  }

  private def handlePost() = formPlayer { (player) =>
    queryGame(directory, system, askTimeout) { (code, room, state) =>
      state match {
        case _: Game.State.WaitingState => join(player, code, room)
        case _ =>
          complete(
            StatusCodes.BadRequest,
            "Game already in progress. Can't join."
          )
      }
    }
  }

  private def join(
      player: Name,
      code: String,
      room: ActorRef[Game.Msg]
  ) = {
    room ! Game.Msg.PlayerJoin(player)
    onSuccess(room.ask(Game.Msg.GetState)) { (state) =>
      val query = Map(
        "code" -> code,
        "name" -> player.toString
      )
      redirect(s"/player?${query.queryString()}", StatusCodes.SeeOther)
    }
  }

  def joinUrlStr(code: String): String = {
    val query = Map("code" -> code)
    new URL(
      publicRoot.getProtocol,
      publicRoot.getHost,
      publicRoot.getPort,
      s"/join?${query.queryString()}"
    ).toString
  }

  private def handleQr() = queryGame(directory, system, askTimeout) {
    (code, room, state) =>
      import QRCodeToSVGWriter.SVGGraphics2DMarshaller

      val qr = Encoder.encode(joinUrlStr(code), ErrorCorrectionLevel.M)
      val svg = QRCodeToSVGWriter.render(qr)
      complete(svg)
  }
}
