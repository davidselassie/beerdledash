import HtmlRenderer.htmlContent
import ScalatagsMarshallers._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{complete, get, pathPrefix}
import akka.http.scaladsl.server.Route
import scalatags.Text.implicits._
import scalatags.Text.tags.{h1, h2, li, p, ul}
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
          p("It's suspiciously like Balderdash for beer descriptions."),
          h2("Setup"),
          p(
            "Each player brings a different beer to share. ",
            "We'll play one round per beer, with the player who brought the beer acting as host."
          ),
          h2("Each Round"),
          p(
            "Everyone tastes the host's beer. ",
            "The host secretly looks up the real beer description from the brewer. ",
            "Everyone else secretly writes a fake beer description, trying to trick others into thinking their fake description is the real one. ",
            "I'll shuffle all the descriptions and the host will read them aloud. ",
            "Everyone but the host votes for the description they think was real to score points."
          ),
          h2("Scoring"),
          ul(
            li(
              "1 point per vote to the writer of each fake description for tricking them"
            ),
            li(
              "2 points to each voter for the real description for being clever"
            ),
            li(
              "3 points to the host if nobody votes for the real description for being lucky"
            )
          ),
          p(
            "Scores are tallied up at the end of all the rounds and the winner has the most points."
          )
        )
      )
    )
  }
}
