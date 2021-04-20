import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorSystem, Behavior, PostStop}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.server.Directives.concat

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Random, Success}

object Guardian {

  sealed trait Msg

  object Msg {

    final case class Started(http: ServerBinding) extends Msg

    final case class StartFailed(ex: Throwable) extends Msg
  }

  def apply(config: EnvConfig): Behavior[Msg] = Behaviors.setup { (ctx) =>
    implicit val system: ActorSystem[Nothing] = ctx.system
    implicit val ec: ExecutionContext = system.executionContext

    val random = new Random()
    val directory = ctx.spawn(Directory(random), "directory")

    ctx.log.info("Public root is {}", config.publicRoot)
    val joinRoute = new JoinRoute(config.publicRoot, directory)

    val routes = Seq(
      new IndexRoute(),
      new HowToPlayRoute(),
      new CreateRoute(directory),
      joinRoute,
      new GameRoute(joinRoute, directory)
    ) ++ config.staticDir.map { (staticDir) =>
      ctx.log.info("Static dir is {}", staticDir)
      new StaticRoute(staticDir)
    }

    ctx.log.info(
      "Starting server on interface for {}:{}",
      config.host,
      config.privatePort
    )
    val httpFuture = Http()
      .newServerAt(config.host, config.privatePort)
      .bind(concat(routes.map(_.route): _*))
    ctx.pipeToSelf(httpFuture) {
      case Success(http) => Msg.Started(http)
      case Failure(ex)   => Msg.StartFailed(ex)
    }

    new Guardian(ctx).starting()
  }
}

private class Guardian(ctx: ActorContext[Guardian.Msg]) {
  import Guardian._

  private def starting() = Behaviors.receiveMessage[Msg] {
    case Msg.Started(http) => {
      ctx.log.info("Server started")
      running(http)
    }
    case Msg.StartFailed(ex) => throw ex
  }

  private def running(http: ServerBinding) = Behaviors
    .receiveMessage[Msg] {
      case Msg.StartFailed(ex) => throw ex
      case _                   => Behaviors.same
    }
    .receiveSignal {
      case (_, PostStop) => {
        http.unbind()
        ctx.log.info("Server stopped")
        Behaviors.same
      }
    }
}
