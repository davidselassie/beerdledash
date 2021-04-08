import akka.http.scaladsl.server.Directives.{getFromDirectory, pathPrefix}
import akka.http.scaladsl.server.Route

class StaticRoute(staticDir: String) extends RouteObj {

  override val route: Route = pathPrefix("static") {
    getFromDirectory(staticDir)
  }
}
