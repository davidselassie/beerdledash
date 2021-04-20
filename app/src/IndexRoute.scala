import AutoTags.autocapitalize
import HtmlRenderer.htmlContent
import ScalatagsMarshallers._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{complete, get, pathSingleSlash}
import akka.http.scaladsl.server.Route
import scalatags.Text.attrs.{
  `type`,
  action,
  method,
  name,
  placeholder,
  required,
  value,
  href
}
import scalatags.Text.implicits._
import scalatags.Text.tags.{a, form, h1, h2, input, label, p}
import scalatags.Text.tags2.main

class IndexRoute() extends RouteObj {

  override val route: Route = pathSingleSlash {
    get(handleGet())
  }

  private def handleGet() = {
    complete(
      StatusCodes.OK,
      htmlContent(
        main(
          h1("Welcome to Beerdledash!"),
          p("It's like Balderdash for beer descriptions."),
          p(a(href := "/howto")("Learn how to play.")),
          form(action := "/create", method := "POST")(
            h2("New Game"),
            input(`type` := "submit", value := "Create")
          ),
          form(action := "/join", method := "GET")(
            h2("Join Game"),
            label(
              "Game code? ",
              input(
                `type` := "text",
                name := "code",
                required,
                autocapitalize := "characters",
                placeholder := "ABCD"
              )
            ),
            p(input(`type` := "submit", value := "Join"))
          )
        )
      )
    )
  }
}
