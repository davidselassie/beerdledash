import akka.actor.typed.ActorSystem
import sun.misc.Signal

import java.net.URL
import scala.concurrent.ExecutionContext
import scala.sys.exit

object App {

  def main(args: Array[String]): Unit = {
    val config = args match {
      case Array(host, privatePortStr, publicRoot) =>
        EnvConfig(
          host,
          Integer.parseInt(privatePortStr),
          new URL(publicRoot),
          None
        )
      case Array(host, privatePortStr, publicRoot, staticDir) =>
        EnvConfig(
          host,
          Integer.parseInt(privatePortStr),
          new URL(publicRoot),
          Some(staticDir)
        )
      case _ =>
        throw new IllegalArgumentException(
          "USAGE: BeerdledashApp HOST PRIVATE_PORT PUBLIC_ROOT [STATIC_DIR]"
        )
    }

    val system = ActorSystem(Guardian(config), "main")
    implicit val ec: ExecutionContext = system.executionContext

    Signal.handle(
      new Signal("INT"),
      (_) => {
        system.terminate()
        system.whenTerminated.onComplete((_) => exit(0))
      }
    )
  }
}
