import GameTypes.Code
import HtmlRenderer.htmlContent
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.{Rejection, RejectionHandler}
import scalatags.Text.tags.{a, h1, p, code => codetag}
import scalatags.Text.tags2.main
import scalatags.Text.implicits._
import ScalatagsMarshallers._
import scalatags.Text.attrs.href

sealed trait GameRejection extends Rejection

object GameRejection {

  final case class RoomNotFoundRejection(code: Code) extends GameRejection

  final case class GameAlreadyStartedRejection(code: Code) extends GameRejection

  implicit val Handler: RejectionHandler = RejectionHandler
    .newBuilder()
    .handleAll[GameRejection] {
      case RoomNotFoundRejection(code) :: _ => {
        complete(
          StatusCodes.NotFound,
          htmlContent(
            main(
              h1("No Game ", codetag(code.toString)),
              p("There's no game with code ", codetag(code.toString), ".")
            )
          )
        )
      }
      case GameAlreadyStartedRejection(code) :: _ => {
        complete(
          StatusCodes.BadRequest,
          htmlContent(
            main(
              h1("Game ", codetag(code.toString), " Already Started"),
              p("You can only join a game before it starts."),
              p(
                "If someone accidentally pressed Start Game, ",
                a(href := "/create")("create a new game"),
                " and have everyone join with that new code."
              )
            )
          )
        )
      }
    }
    .result()
    .withFallback(RejectionHandler.default)
}
