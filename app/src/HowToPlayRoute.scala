import HtmlRenderer.htmlContent
import ScalatagsMarshallers._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{complete, get, pathPrefix}
import akka.http.scaladsl.server.Route
import scalatags.Text.implicits._
import scalatags.Text.tags.{h1, p}
import scalatags.Text.tags2.main

class HowToPlayRoute() extends RouteObj {

  override val route: Route = pathPrefix("howto") {
    get(handleGet())
  }

  private def handleGet() = {
    complete(
      StatusCodes.OK,
      htmlContent(
        main(
          h1("How to Play Beerdledash"),
          p("It's like Balderdash for beer descriptions.")
        )
      )
    )
  }
}
