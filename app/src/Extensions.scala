import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Random

object Extensions {

  implicit class SelectEx(random: Random) {

    def select[T](xs: Seq[T]): T = {
      xs(random.nextInt(xs.size))
    }
  }

  implicit class QueryStringEx(m: Map[String, String]) {

    def queryString(): String = {
      m.map { case (k, v) =>
        s"${URLEncoder.encode(k, StandardCharsets.UTF_8.name())}=${URLEncoder
          .encode(v, StandardCharsets.UTF_8.name())}"
      }.mkString("&")
    }
  }
}
