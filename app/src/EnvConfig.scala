import java.net.URL

final case class EnvConfig(
    host: String,
    privatePort: Int,
    publicRoot: URL,
    staticDir: Option[String]
)
