import akka.http.scaladsl.server.Route
import akka.util.Timeout

import scala.concurrent.duration.SECONDS

trait RouteObj {

  val route: Route

  implicit val askTimeout: Timeout = Timeout(1, SECONDS)
}
