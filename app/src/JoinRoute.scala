import Extensions.QueryStringEx
import GameDirective.{formPlayer, queryGame}
import GameRejection.GameAlreadyStartedRejection
import GameTypes.{Code, Name}
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
  redirect,
  reject
}
import akka.http.scaladsl.server.Route
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import com.google.zxing.qrcode.encoder.Encoder
import scalatags.Text.attrs.{
  `type`,
  autocomplete,
  maxlength,
  method,
  name,
  placeholder,
  required,
  value
}
import scalatags.Text.implicits._
import scalatags.Text.tags.{form, h1, input, label, p, code => codetag}
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

  private def bodyContent(code: Code) = htmlContent(
    main(
      form(method := "POST")(
        h1("Join Game ", codetag(code.toString)),
        label(
          "What's your name? ",
          input(
            `type` := "text",
            name := "name",
            autocomplete := "given-name",
            required,
            maxlength := Name.MaxLength,
            placeholder := "Rob"
          )
        ),
        p(input(`type` := "submit", value := "Join"))
      )
    )
  )

  private def handleGet() = queryGame(directory, system, askTimeout) {
    (code, room, state) =>
      state match {
        case _: Game.State.WaitingState =>
          complete(StatusCodes.OK, bodyContent(code))
        case _ => reject(GameAlreadyStartedRejection(code))
      }
  }

  private def handlePost() = formPlayer { (player) =>
    queryGame(directory, system, askTimeout) { (code, room, state) =>
      state match {
        case _: Game.State.WaitingState => {
          room ! Game.Msg.PlayerJoin(player)
          onSuccess(room.ask(Game.Msg.GetState)) { (state) =>
            val query = Map(
              "code" -> code.toString,
              "name" -> player.toString
            )
            redirect(s"/game?${query.queryString()}", StatusCodes.SeeOther)
          }
        }
        case _ => reject(GameAlreadyStartedRejection(code))
      }
    }
  }

  def joinUrlStr(code: Code): String = {
    val query = Map("code" -> code.toString)
    new URL(
      publicRoot.getProtocol,
      publicRoot.getHost,
      publicRoot.getPort,
      s"/join?${query.queryString()}"
    ).toString
  }

  private def handleQr() = queryGame(directory, system, askTimeout) {
    (code, room, state) =>
      import QRCodeToSVGWriter.SvgMarshaller

      val qr = Encoder.encode(joinUrlStr(code), ErrorCorrectionLevel.M)
      val svg = QRCodeToSVGWriter.render(qr)
      complete(svg)
  }
}
