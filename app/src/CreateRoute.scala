import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{
  onSuccess,
  pathPrefix,
  post,
  redirect
}
import akka.http.scaladsl.server.Route
import Extensions.QueryStringEx
import akka.actor.typed.scaladsl.AskPattern.{Askable, schedulerFromActorSystem}

class CreateRoute(directory: ActorRef[Directory.Msg])(implicit
    system: ActorSystem[_]
) extends RouteObj {

  override val route: Route = pathPrefix("create") {
    post(handlePost())
  }

  private def handlePost() =
    onSuccess(directory.ask((replyTo) => Directory.Create(replyTo))) {
      case Directory.Created(code, _) => {
        val query = Map("code" -> code)
        redirect(s"/server?${query.queryString()}", StatusCodes.SeeOther)
      }
    }
}
